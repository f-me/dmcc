{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

{-|

CSTA XML API implementation for third party call control and
monitoring.

TODO:

- Logging

-}

module CSTA
  ( Session
  , startSession
  , stopSession

  , controlAgent
  , releaseAgent

  , Action(..)
  , agentAction

  , Rs.Event
  , handleEvents
  , getAgentCalls

  , module CSTA.Types
  )

where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Aeson.TH
import           Data.Data
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import           Data.Text (Text, unpack)

import           Data.Time.Clock

import           System.IO
import           Network

import           CSTA.Types
import           CSTA.XML.Request (Request)
import qualified CSTA.XML.Request as Rq
import           CSTA.XML.Response (Response)
import qualified CSTA.XML.Response as Rs
import qualified CSTA.XML.Raw as Raw


-- | Low-level CSTA XML API plumbing (message loop).
data CSTAHandle = CSTAHandle
  { socket :: Handle
  , readThread :: ThreadId
    -- ^ CSTA response reader thread.
  , procThread :: ThreadId
    -- ^ Response handler/synchronous requests worker thread.
  , syncResponses :: TVar (IntMap.IntMap (TMVar Response))
  , invokeId :: TVar Int
    -- ^ Request/response counter.
  }


-- | Actions performable by a controlled agent.
data Action = MakeCall{number :: Extension}
            | AnswerCall{callId :: CallId}
            | EndCall{callId :: CallId}
            deriving Show


$(deriveFromJSON
  defaultOptions{sumEncoding = defaultTaggedObject{tagFieldName="action"}}
  ''Action)


newtype AgentId =
  AgentId (SwitchName, Extension)
  deriving (Data, Typeable, Eq, Ord, Show)


-- | An agent controlled by a CSTA API session.
data Agent = Agent
  { deviceId :: DeviceId
  , monitorId :: Text
  , actionChan :: TChan Action
  -- ^ Actions performed by the agent.
  , actionThread :: ThreadId
  , inputChan :: TChan Rs.Event
  -- ^ Input chan for XML events produced by this agent's monitor.
  , inputThread :: ThreadId
  , eventChan :: TChan Rs.Event
  -- ^ Broadcasting chan with agent events.
  , calls :: TVar (Map.Map CallId Call)
  }


instance Show Agent where
  show (Agent (DeviceId d) m _ _ _ _ _ _) =
    "Agent{deviceId=" ++ unpack d ++
    ", monitorId=" ++ unpack m ++
    "}"


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


type AgentHandle = (AgentId, Session)


data LoopEvent
  = CSTARsp Response
  | Timeout
  | ReadError
  deriving Show


data Error = Error String
           | UnknownAgent AgentId
           deriving (Data, Typeable, Show)


instance (Exception Error)


--FIXME: handle network errors
startSession :: String
             -> Int
             -> Text
             -> Text
             -> IO Session
startSession host port user pass = withSocketsDo $ do
  sock <- connectTo host (PortNumber $ fromIntegral port)
  hSetBuffering sock NoBuffering

  -- Request/response plumbing
  msgChan <- newTChanIO
  readThread <-
    forkIO $ forever $
    Raw.readResponse sock >>=
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
  let h = CSTAHandle sock readThread procThread syncResponses invokeId

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
    sendRequestAsyncRaw sock invokeId
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

  sendRequestSync avayaHandle $
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
  Raw.sendRequest socket ix rq
  atomically $ takeTMVar var


sendRequestAsync :: CSTAHandle -> Request -> IO ()
sendRequestAsync (CSTAHandle{..}) rq =
  sendRequestAsyncRaw socket invokeId rq


sendRequestAsyncRaw :: Handle -> TVar Int -> Request -> IO ()
sendRequestAsyncRaw sock invoke rq = do
  ix <- atomically $ do
    modifyTVar' invoke ((`mod` 9999).(+1))
    readTVar invoke
  -- FIXME: handle error
  Raw.sendRequest sock ix rq


agentAction :: Action -> AgentHandle -> IO ()
agentAction cmd (aid, as) = do
  ags <- readTVarIO (agents as)
  case Map.lookup aid ags of
    Just (Agent{..}) -> atomically $ writeTChan actionChan cmd
    Nothing -> error "No such agent"


placeAgentLock :: AgentHandle -> STM ()
placeAgentLock (aid, as) = modifyTVar' (agentLocks as) (Set.insert aid)


releaseAgentLock :: AgentHandle -> IO ()
releaseAgentLock (aid, as) =
  atomically $ modifyTVar' (agentLocks as) (Set.delete aid)


-- | Enable an active agent to be monitored and controlled through
-- CSTA API. If the agent has already been registered, return the old
-- entry (it's safe to call this function for the same agent multiple
-- times).
controlAgent :: SwitchName
             -> Extension
             -> Session
             -> IO AgentHandle
controlAgent switch ext as = do
  let aid = AgentId (switch, ext)
  -- Check if agent already exists
  prev <- atomically $ do
    locks <- readTVar (agentLocks as)
    case Set.member aid locks of
      True -> retry
      False -> do
        ags <- readTVar (agents as)
        case Map.member aid ags of
          True -> return $ Just aid
          -- Prevent parallel operation on the same agent
          False -> placeAgentLock (aid, as) >> return Nothing

  case prev of
    Just a -> return (a, as)
    Nothing ->
      handle (\e ->
                releaseAgentLock (aid, as) >>
                (throwIO (e :: IOException))) $
      do
        Rs.GetDeviceIdResponse{..} <-
          sendRequestSync (avayaHandle as) $
          Rq.GetDeviceId
          { switchName = switch
          , extension = ext
          }

        -- Setup action and event processing for this agent

        actionChan <- newTChanIO
        actionThread <-
          forkIO $ forever $
          (atomically $ readTChan actionChan) >>=
          processAgentAction aid device as

        eventChan <- newBroadcastTChanIO
        calls <- newTVarIO Map.empty

        inputChan <- newTChanIO
        inputThread <-
          forkIO $ forever $
          (atomically $ readTChan inputChan) >>=
          processAgentEvent device calls eventChan


        Rs.MonitorStartResponse{..} <-
          sendRequestSync (avayaHandle as) $
          Rq.MonitorStart
          { deviceObject = device
          , acceptedProtocol = protocolVersion as
          }
        let ag = Agent
                 device
                 monitorCrossRefID
                 actionChan
                 actionThread
                 inputChan
                 inputThread
                 eventChan
                 calls
        atomically $ modifyTVar' (agents as) (Map.insert aid ag)
        releaseAgentLock (aid, as)
        return (aid, as)


-- | Translate agent actions into actual CSTA API requests.
processAgentAction :: AgentId -> DeviceId -> Session -> Action -> IO ()
processAgentAction (AgentId (switch, _)) device as action =
  case action of
    MakeCall toNumber -> do
      rspDest <-
        sendRequestSync (avayaHandle as) $
        Rq.GetThirdPartyDeviceId
        -- Assume destination switch is the same as agent's
        { switchName = switch
        , extension = toNumber
        }
      sendRequestAsync (avayaHandle as) $
        Rq.MakeCall
        device
        (Rs.device rspDest)
        (protocolVersion as)
    AnswerCall callId ->
      sendRequestAsync (avayaHandle as) $
        Rq.AnswerCall
        device
        callId
        (protocolVersion as)
    EndCall callId ->
      sendRequestAsync (avayaHandle as) $
        Rq.ClearConnection
        device
        callId
        (protocolVersion as)


-- | Process CSTA API events for this agent to change its state and
-- broadcast events further.
processAgentEvent :: DeviceId
                  -> TVar (Map.Map CallId Call)
                  -> TChan Rs.Event
                  -> Rs.Event
                  -> IO ()
processAgentEvent device calls eventChan ev = do
  case ev of
    -- New call
    Rs.DeliveredEvent{..} -> do
      now <- getCurrentTime
      let (dir, interloc) = if callingDevice == device
                             then (Out, calledDevice)
                             else (In, callingDevice)
          call = Call dir now interloc Nothing
      atomically $ do
        modifyTVar calls (Map.insert callId call)
    Rs.EstablishedEvent{..} -> do
      now <- getCurrentTime
      atomically $
        modifyTVar calls $
          \m ->
            case Map.lookup callId m of
              Just call ->
                Map.insert callId call{answered = Just now} m
              Nothing ->
                error "Established connection to undelivered call"
    Rs.ConnectionClearedEvent{..} -> do
      atomically $ modifyTVar calls $ Map.delete callId
    Rs.UnknownEvent -> return ()
  atomically $ writeTChan eventChan ev


-- | Forget about an agent, releasing his device and monitors.
releaseAgent :: AgentHandle
             -> IO ()
releaseAgent (aid, as) = do
  prev <- atomically $ do
    locks <- readTVar (agentLocks as)
    case Set.member aid locks of
      True -> retry
      False -> do
        ags <- readTVar (agents as)
        case Map.lookup aid ags of
          Just ag -> placeAgentLock (aid, as) >> (return $ Just ag)
          Nothing -> return Nothing

  case prev of
    Nothing -> throwIO $ UnknownAgent aid
    Just ag ->
      handle (\e ->
                releaseAgentLock (aid, as) >>
                (throwIO (e :: IOException))) $
      do
        sendRequestSync (avayaHandle as) $
          Rq.MonitorStop
          { acceptedProtocol = protocolVersion as
          , monitorCrossRefID = monitorId ag
          }
        sendRequestSync (avayaHandle as) $
          Rq.ReleaseDeviceId{device = deviceId ag}
        killThread (actionThread ag)
        atomically $ modifyTVar' (agents as) (Map.delete aid)
        releaseAgentLock (aid, as)
        return ()


handleEvents :: AgentHandle -> (Rs.Event -> IO ()) -> IO ThreadId
handleEvents (aid, as) handler = do
  ags <- readTVarIO (agents as)
  case Map.lookup aid ags of
    Nothing -> throwIO $ UnknownAgent aid
    Just (Agent{..}) ->
      do
        sub <- atomically $ dupTChan eventChan
        tid <- forkIO $ forever $ do
          ev <- atomically $ readTChan sub
          handler ev
        return tid


getAgentCalls :: AgentHandle
              -> IO (Map.Map CallId Call)
getAgentCalls (aid, as) = do
  ags <- readTVarIO (agents as)
  case Map.lookup aid ags of
    Nothing -> throwIO $ UnknownAgent aid
    Just (Agent{..}) -> readTVarIO calls
