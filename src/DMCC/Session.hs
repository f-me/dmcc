{-# LANGUAGE TupleSections #-}

{-|

DMCC session handling.

-}

module DMCC.Session

where

import           Control.Arrow
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Functor

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
import qualified Network.HTTP.Client as HTTP
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
  , invokeId :: TVar Int
  -- ^ Request/response counter.
  , syncResponses :: TVar (IntMap.IntMap (TMVar Response))
  , agentRequests :: TVar (IntMap.IntMap AgentId)
  -- ^ Keeps track of which request has has been issued by what agent
  -- (if there's one) until its response arrives.
  , loggingOptions :: Maybe LoggingOptions
  }


data Session = Session
  { sessionId :: Text
  , pingThread :: ThreadId
  , protocolVersion :: Text
  -- ^ Action worker thread.
  , dmccHandle :: DMCCHandle
  , webHook :: Maybe (HTTP.Request, HTTP.Manager)
  -- ^ Web hook handler URL and manager.
  , agents :: TVar (Map.Map AgentId Agent)
  , agentLocks :: TVar (Set.Set AgentId)
  }


instance Show Session where
  show as =
    "Session{sessionId=" ++ (unpack $ sessionId as) ++
    ", protocolVersion=" ++ (unpack $ protocolVersion as) ++
    "}"


instance Eq Session where
  s1 == s2 = (sessionId s1) == (sessionId s2)


instance Ord Session where
  compare s1 s2 = compare (sessionId s1) (sessionId s2)


data LoopEvent
  = DMCCRsp Response
  | Timeout
  | ReadError
  deriving Show


defaultLoggingOptions :: LoggingOptions
defaultLoggingOptions = LoggingOptions "dmcc-lib"


--FIXME: handle network errors
startSession :: (String, PortNumber)
             -- ^ Host and port of AES server.
             -> ConnectionType
             -- ^ Use TLS.
             -> Text
             -- ^ DMCC API user.
             -> Text
             -- ^ DMCC API password.
             -> Maybe String
             -- ^ Web hook URL.
             -> Maybe LoggingOptions
             -> IO Session
startSession (host, port) conn user pass whUrl lopts = withOpenSSL $ do
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
  agentRequests <- newTVarIO IntMap.empty
  agents <- newTVarIO Map.empty

  procThread <- forkIO $ forever $ do
    (msg, invokeId) <- atomically $ readTChan msgChan
    -- TODO Check agent locks?
    ags <- readTVarIO agents
    case msg of
      DMCCRsp rsp -> do
        -- Return response if the request was synchronous
        sync' <- atomically $ do
          srs <- readTVar syncResponses
          modifyTVar' syncResponses (IntMap.delete invokeId)
          return $ IntMap.lookup invokeId srs
        case sync' of
          Just sync -> void $ atomically $ tryPutTMVar sync rsp
          Nothing -> return ()
        -- Redirect events and request errors to matching agent
        ag' <- case rsp of
                 Rs.EventResponse monId _ ->
                   return $ find (\a -> monId == monitorId a) $ Map.elems ags
                 Rs.CSTAErrorCodeResponse _ -> do
                   aid <- atomically $ do
                     ars <- readTVar agentRequests
                     modifyTVar' agentRequests (IntMap.delete invokeId)
                     return $ IntMap.lookup invokeId ars
                   return $ (`Map.lookup` ags) =<< aid
                 _ -> return Nothing
        case ag' of
          Just ag -> atomically $ writeTChan (inputChan ag) rsp
          -- Error/event received for an unknown agent?
          Nothing -> return ()
      _ -> return ()

  invokeId <- newTVarIO 0
  agLocks <- newTVarIO Set.empty
  let h = DMCCHandle
          (istream, ostream)
          cl
          readThread
          procThread
          invokeId
          syncResponses
          agentRequests
          lopts

  Rs.StartApplicationSessionPosResponse{..} <- sendRequestSync h Nothing
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
    sendRequestAsyncRaw lopts ostream invokeId Nothing
      $ Rq.ResetApplicationSessionTimer
            { sessionId = sessionID
            , requestedSessionDuration = actualSessionDuration
            }

  wh <- case whUrl of
          Just url -> do
            mgr <- HTTP.newManager HTTP.defaultManagerSettings
            req <- HTTP.parseUrl url
            return $ Just (req, mgr)
          Nothing -> return Nothing

  return $
    Session
    sessionID
    pingThread
    actualProtocolVersion
    h
    wh
    agents
    agLocks


-- | TODO Agent releasing notice
stopSession :: Session -> IO ()
stopSession as@(Session{..}) = do
  -- Release all agents
  ags <- readTVarIO agents
  mapM_ releaseAgent $ zip (Map.keys ags) (repeat as)

  sendRequestAsync dmccHandle Nothing $
    Rq.StopApplicationSession{sessionID = sessionId}
  killThread pingThread
  killThread $ procThread dmccHandle
  killThread $ readThread dmccHandle
  write Nothing (snd $ streams $ dmccHandle)
  cleanup dmccHandle


sendRequestSync :: DMCCHandle -> Maybe AgentId -> Request -> IO Response
sendRequestSync (DMCCHandle{..}) aid rq = do
  (ix,var) <- atomically $ do
    modifyTVar' invokeId ((`mod` 9999).(+1))
    ix <- readTVar invokeId
    var <- newEmptyTMVar
    modifyTVar' syncResponses (IntMap.insert ix var)
    case aid of
      Just a -> modifyTVar' agentRequests (IntMap.insert ix a)
      Nothing -> return ()
    return (ix, var)
  -- FIXME: handle exceptions
  Raw.sendRequest loggingOptions (snd streams) ix rq
  atomically $ takeTMVar var


sendRequestAsync :: DMCCHandle -> Maybe AgentId -> Request -> IO ()
sendRequestAsync (DMCCHandle{..}) aid rq =
  sendRequestAsyncRaw loggingOptions (snd streams) invokeId a' rq
  where
    a' = (agentRequests, ) <$> aid


sendRequestAsyncRaw :: Maybe LoggingOptions
                    -> OutputStream ByteString
                    -> TVar Int
                    -> Maybe (TVar (IntMap.IntMap AgentId), AgentId)
                    -> Request
                    -> IO ()
sendRequestAsyncRaw lopts ostream invoke ar rq = do
  ix <- atomically $ do
    modifyTVar' invoke ((`mod` 9999).(+1))
    ix' <- readTVar invoke
    case ar of
      Just (agentRequests, a) -> modifyTVar' agentRequests (IntMap.insert ix' a)
      Nothing -> return ()
    return ix'
  -- FIXME: handle exceptions
  Raw.sendRequest lopts ostream ix rq
