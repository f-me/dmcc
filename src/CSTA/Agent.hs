{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Agents in a CSTA API session enable third-party call control over
devices.

-}

module CSTA.Agent

where

import           Control.Exception
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Aeson.TH
import           Data.Data
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (Text, unpack)

import           Data.Time.Clock

import           CSTA.Session
import           CSTA.Types
import qualified CSTA.XML.Request as Rq
import qualified CSTA.XML.Response as Rs


-- | Actions performable by a controlled agent.
data Action = MakeCall{number :: Extension}
            | AnswerCall{callId :: CallId}
            | EndCall{callId :: CallId}
            deriving Show


$(deriveFromJSON
  defaultOptions{sumEncoding = defaultTaggedObject{tagFieldName="action"}}
  ''Action)


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


type AgentHandle = (AgentId, Session)


data AgentError = Error String
                | UnknownAgent AgentId
                deriving (Data, Typeable, Show)


instance (Exception AgentError)


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
