{-|

CSTA session handling.

-}

module CSTA.Session

where

import           Control.Arrow
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import           Data.Text (Text, unpack)

import           System.IO
import           Network

import           CSTA.Types
import           CSTA.XML.Request (Request)
import qualified CSTA.XML.Request as Rq
import           CSTA.XML.Response (Response)
import qualified CSTA.XML.Response as Rs
import qualified CSTA.XML.Raw as Raw

import {-# SOURCE #-} CSTA.Agent


-- | Low-level AES API plumbing.
data CSTAHandle = CSTAHandle
  { socket :: Handle
  , readThread :: ThreadId
    -- ^ CSTA response reader thread.
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
  , avayaHandle :: CSTAHandle
  , agents :: TVar (Map.Map AgentId Agent)
  , agentLocks :: TVar (Set.Set AgentId)
  }


instance Show Session where
  show as =
    "Session{sessionId=" ++ (unpack $ sessionId as) ++
    ", protocolVersion=" ++ (unpack $ protocolVersion as) ++
    "}"


data LoopEvent
  = CSTARsp Response
  | Timeout
  | ReadError
  | ShutdownRequested
  deriving Show



defaultLoggingOptions :: LoggingOptions
defaultLoggingOptions = LoggingOptions "csta-lib" False


--FIXME: handle network errors
startSession :: String
             -> Int
             -> Text
             -> Text
             -> Maybe LoggingOptions
             -> IO Session
startSession host port user pass lopts = withSocketsDo $ do
  sock <- connectTo host (PortNumber $ fromIntegral port)
  hSetBuffering sock NoBuffering

  -- Request/response plumbing
  msgChan <- newTChanIO
  readThread <-
    forkIO $ forever $
    Raw.readResponse lopts sock >>=
    atomically . writeTChan msgChan . first CSTARsp

  syncResponses <- newTVarIO IntMap.empty
  agents <- newTVarIO Map.empty

  procThread <- forkIO $ forever $ do
    (msg, invokeId) <- atomically $ readTChan msgChan
    case msg of
      -- Redirect events to matching agent
      (CSTARsp (Rs.EventResponse monId ev)) -> do
        -- TODO Check agent locks?
        ags <- readTVarIO agents
        case find (\a -> monId == monitorId a) $ Map.elems ags of
          Just ag -> atomically $ writeTChan (inputChan ag) ev
          -- Event received for unknown agent?
          Nothing -> return ()
      CSTARsp rsp -> do
        syncs <- readTVarIO syncResponses
        case IntMap.lookup invokeId syncs of
          Nothing -> return ()
          Just sync -> void $ atomically $ tryPutTMVar sync rsp
      _ -> return ()

  invokeId <- newTVarIO 0
  agLocks <- newTVarIO Set.empty
  let h = CSTAHandle sock readThread procThread syncResponses invokeId lopts

  Rs.StartApplicationSessionPosResponse{..} <- sendRequestSync h
    $ Rq.StartApplicationSession
      { applicationId = ""
      , requestedProtocolVersion = Rq.V4_2
      , userName = user
      , password = pass
      , sessionCleanupDelay = 80
      , requestedSessionDuration = 80
      }

  -- Keep session alive
  pingThread <- forkIO $ forever $ do
    threadDelay $ actualSessionDuration * 500 * 1000
    sendRequestAsyncRaw lopts sock invokeId
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

  sendRequestAsync avayaHandle $
    Rq.StopApplicationSession{sessionID = sessionId}
  killThread pingThread
  killThread $ procThread avayaHandle
  killThread $ readThread avayaHandle
  hClose $ socket avayaHandle


sendRequestSync :: CSTAHandle -> Request -> IO Response
sendRequestSync (CSTAHandle{..}) rq = do
  (ix,var) <- atomically $ do
    modifyTVar' invokeId ((`mod` 9999).(+1))
    ix <- readTVar invokeId
    var <- newEmptyTMVar
    modifyTVar' syncResponses (IntMap.insert ix var)
    return (ix,var)
  -- FIXME: handle error
  Raw.sendRequest loggingOptions socket ix rq
  atomically $ takeTMVar var


sendRequestAsync :: CSTAHandle -> Request -> IO ()
sendRequestAsync (CSTAHandle{..}) rq =
  sendRequestAsyncRaw loggingOptions socket invokeId rq


sendRequestAsyncRaw :: Maybe LoggingOptions
                    -> Handle
                    -> TVar Int
                    -> Request
                    -> IO ()
sendRequestAsyncRaw lopts sock invoke rq = do
  ix <- atomically $ do
    modifyTVar' invoke ((`mod` 9999).(+1))
    readTVar invoke
  -- FIXME: handle error
  Raw.sendRequest lopts sock ix rq
