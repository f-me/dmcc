{-|

DMCC session handling.

-}

module DMCC.Session

where

import           Control.Arrow
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.ByteString (ByteString)
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import           Data.Text (Text, unpack)

import           System.IO.Streams (InputStream, OutputStream, write)
import           System.IO.Streams.Handle
import qualified System.IO.Streams.SSL as SSLStreams

import           Network
import           Network.Socket
import           OpenSSL
import qualified OpenSSL.Session as SSL
import           System.IO

import           DMCC.Types
import           DMCC.XML.Request (Request)
import qualified DMCC.XML.Request as Rq
import           DMCC.XML.Response (Response)
import qualified DMCC.XML.Response as Rs
import qualified DMCC.XML.Raw as Raw

import {-# SOURCE #-} DMCC.Agent


data ConnectionType = Plain
                    | TLS { caDir :: Maybe FilePath }


-- | Low-level AES API plumbing.
data DMCCHandle = DMCCHandle
  { streams :: (InputStream ByteString, OutputStream ByteString)
  , cleanup :: IO ()
  -- ^ Properly close underlying connection.
  , readThread :: ThreadId
  -- ^ DMCC response reader thread.
  , procThread :: ThreadId
  -- ^ Response handler/synchronous requests worker thread.
  , syncResponses :: TVar (IntMap.IntMap (TMVar Response))
  , invokeId :: TVar Int
  -- ^ Request/response counter.
  , loggingOptions :: Maybe LoggingOptions
  }


data Session = Session
  { sessionId :: Text
  , pingThread :: ThreadId
  , protocolVersion :: Text
  -- ^ Action worker thread.
  , dmccHandle :: DMCCHandle
  , agents :: TVar (Map.Map AgentId Agent)
  , agentLocks :: TVar (Set.Set AgentId)
  }


instance Show Session where
  show as =
    "Session{sessionId=" ++ (unpack $ sessionId as) ++
    ", protocolVersion=" ++ (unpack $ protocolVersion as) ++
    "}"


data LoopEvent
  = DMCCRsp Response
  | Timeout
  | ReadError
  | ShutdownRequested
  deriving Show


defaultLoggingOptions :: LoggingOptions
defaultLoggingOptions = LoggingOptions "dmcc-lib"


--FIXME: handle network errors
startSession :: String
             -> PortNumber
             -> ConnectionType
             -- ^ Use TLS.
             -> Text
             -> Text
             -> Maybe LoggingOptions
             -> IO Session
startSession host port conn user pass lopts = withOpenSSL $ do
  (istream, ostream, cl) <-
    case conn of
      Plain -> do
        handle <- connectTo host (PortNumber $ fromIntegral port)
        hSetBuffering handle NoBuffering
        is <- handleToInputStream handle
        os <- handleToOutputStream handle
        let cl = hClose handle
        return (is, os, cl)
      TLS caDir -> do
        sslCtx <- SSL.context
        SSL.contextSetDefaultCiphers sslCtx
        SSL.contextSetVerificationMode sslCtx $
          SSL.VerifyPeer True True Nothing
        maybe (return ()) (SSL.contextSetCADirectory sslCtx) caDir
        (is, os, ssl) <- SSLStreams.connect sslCtx host port
        let cl =
              do
                SSL.shutdown ssl SSL.Unidirectional
                maybe (return ()) close $ SSL.sslSocket ssl
        return (is, os, cl)

  -- Request/response plumbing
  msgChan <- newTChanIO
  readThread <-
    forkIO $ forever $
    Raw.readResponse lopts istream >>=
    atomically . writeTChan msgChan . first DMCCRsp

  syncResponses <- newTVarIO IntMap.empty
  agents <- newTVarIO Map.empty

  procThread <- forkIO $ forever $ do
    (msg, invokeId) <- atomically $ readTChan msgChan
    case msg of
      -- Redirect events to matching agent
      (DMCCRsp (Rs.EventResponse monId ev)) -> do
        -- TODO Check agent locks?
        ags <- readTVarIO agents
        case find (\a -> monId == monitorId a) $ Map.elems ags of
          Just ag -> atomically $ writeTChan (inputChan ag) ev
          -- Event received for unknown agent?
          Nothing -> return ()
      DMCCRsp rsp -> do
        syncs <- readTVarIO syncResponses
        case IntMap.lookup invokeId syncs of
          Nothing -> return ()
          Just sync -> void $ atomically $ tryPutTMVar sync rsp
      _ -> return ()

  invokeId <- newTVarIO 0
  agLocks <- newTVarIO Set.empty
  let h = DMCCHandle
          (istream, ostream)
          cl
          readThread
          procThread
          syncResponses
          invokeId
          lopts

  Rs.StartApplicationSessionPosResponse{..} <- sendRequestSync h
    $ Rq.StartApplicationSession
      { applicationId = ""
      , requestedProtocolVersion = Rq.DMCC_4_2
      , userName = user
      , password = pass
      , sessionCleanupDelay = 80
      , requestedSessionDuration = 80
      }

  -- Keep session alive
  pingThread <- forkIO $ forever $ do
    threadDelay $ actualSessionDuration * 500 * 1000
    sendRequestAsyncRaw lopts ostream invokeId
      $ Rq.ResetApplicationSessionTimer
            { sessionId = sessionID
            , requestedSessionDuration = actualSessionDuration
            }

  return $
    Session
    sessionID
    pingThread
    actualProtocolVersion
    h
    agents
    agLocks


-- | TODO Agent releasing notice
stopSession :: Session -> IO ()
stopSession as@(Session{..}) = do
  -- Release all agents
  ags <- readTVarIO agents
  mapM_ releaseAgent $ zip (Map.keys ags) (repeat as)

  sendRequestAsync dmccHandle $
    Rq.StopApplicationSession{sessionID = sessionId}
  killThread pingThread
  killThread $ procThread dmccHandle
  killThread $ readThread dmccHandle
  write Nothing (snd $ streams $ dmccHandle)
  cleanup dmccHandle


sendRequestSync :: DMCCHandle -> Request -> IO Response
sendRequestSync (DMCCHandle{..}) rq = do
  (ix,var) <- atomically $ do
    modifyTVar' invokeId ((`mod` 9999).(+1))
    ix <- readTVar invokeId
    var <- newEmptyTMVar
    modifyTVar' syncResponses (IntMap.insert ix var)
    return (ix,var)
  -- FIXME: handle error
  Raw.sendRequest loggingOptions (snd streams) ix rq
  atomically $ takeTMVar var


sendRequestAsync :: DMCCHandle -> Request -> IO ()
sendRequestAsync (DMCCHandle{..}) rq =
  sendRequestAsyncRaw loggingOptions (snd streams) invokeId rq


sendRequestAsyncRaw :: Maybe LoggingOptions
                    -> OutputStream ByteString
                    -> TVar Int
                    -> Request
                    -> IO ()
sendRequestAsyncRaw lopts ostream invoke rq = do
  ix <- atomically $ do
    modifyTVar' invoke ((`mod` 9999).(+1))
    readTVar invoke
  -- FIXME: handle error
  Raw.sendRequest lopts ostream ix rq
