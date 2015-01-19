{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Agents in a CSTA API session enable third-party call control over
devices.

-}

module CSTA.Agent

where

import           Control.Exception
import           Control.Lens hiding (Action)
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Aeson as A
import           Data.Aeson.TH
import           Data.CaseInsensitive (original)
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
            | HoldCall{callId :: CallId}
            | RetrieveCall{callId :: CallId}
            | ConferenceCall{activeCall :: CallId, heldCall :: CallId}
            | TransferCall{activeCall :: CallId, heldCall :: CallId}
            | SendDigits{callId :: CallId, digits :: Text}
            deriving Show


$(deriveFromJSON
  defaultOptions{sumEncoding = defaultTaggedObject{tagFieldName="action"}}
  ''Action)


data AgentState = AgentState
    { _calls :: Map.Map CallId Call
    }
    deriving Show


instance ToJSON AgentState where
  toJSON s =
    object [ "calls" A..= (Map.mapKeys (\(CallId t) -> t) $ _calls s)
           ]


$(makeLenses ''AgentState)


-- | An event observed by a controlled agent, along with updated agent
-- state. Events may be used by agent subscribers to provide user with
-- information.
data Event = Event
    { cstaEvent :: Rs.Event
    , newState :: AgentState
    }
    deriving Show


$(deriveToJSON defaultOptions ''Event)


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
    , eventChan :: TChan Event
    -- ^ Broadcasting chan with agent events.
    , state :: TVar AgentState
    }


instance Show Agent where
  show (Agent (DeviceId d) m _ _ _ _ _ _) =
    "Agent{deviceId=" ++ unpack (original d) ++
    ", monitorId=" ++ unpack m ++
    "}"


type AgentHandle = (AgentId, Session)


data AgentError = Error String
                | UnknownAgent AgentId
                deriving (Data, Typeable, Show)


instance (Exception AgentError)


-- | Command an agent to do something.
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
          sendRequestSync (cstaHandle as) $
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
        state <- newTVarIO $ AgentState Map.empty

        inputChan <- newTChanIO
        inputThread <-
          forkIO $ forever $
          (atomically $ readTChan inputChan) >>=
          processAgentEvent device state eventChan


        Rs.MonitorStartResponse{..} <-
          sendRequestSync (cstaHandle as) $
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
                 state
        atomically $ modifyTVar' (agents as) (Map.insert aid ag)
        releaseAgentLock (aid, as)
        return (aid, as)


-- | Translate agent actions into actual CSTA API requests.
processAgentAction :: AgentId -> DeviceId -> Session -> Action -> IO ()
processAgentAction (AgentId (switch, _)) device as action =
  let
    arq = sendRequestAsync (cstaHandle as)
    simpleRequest rq cid =
      arq $ rq device cid (protocolVersion as)
    simpleRequest2 rq cid1 cid2 =
      arq $ rq device cid1 cid2 (protocolVersion as)
  in
  case action of
    MakeCall toNumber -> do
      rspDest <-
        sendRequestSync (cstaHandle as) $
        Rq.GetThirdPartyDeviceId
        -- Assume destination switch is the same as agent's
        { switchName = switch
        , extension = toNumber
        }
      sendRequestAsync (cstaHandle as) $
        Rq.MakeCall
        device
        (Rs.device rspDest)
        (protocolVersion as)
    AnswerCall callId   -> simpleRequest Rq.AnswerCall callId
    HoldCall callId     -> simpleRequest Rq.HoldCall callId
    RetrieveCall callId -> simpleRequest Rq.RetrieveCall callId
    ConferenceCall activeCall heldCall ->
      simpleRequest2 Rq.ConferenceCall activeCall heldCall
    TransferCall activeCall heldCall ->
      simpleRequest2 Rq.TransferCall activeCall heldCall
    EndCall callId      -> simpleRequest Rq.ClearConnection callId
    -- Synchronous to avoid missed digits if actions queue up
    SendDigits{..}  ->
      void $
      sendRequestSync (cstaHandle as) $
      Rq.GenerateDigits digits device callId (protocolVersion as)


-- | Process CSTA API events for this agent to change its state and
-- broadcast events further.
processAgentEvent :: DeviceId
                  -> TVar AgentState
                  -> TChan Event
                  -> Rs.Event
                  -> IO ()
processAgentEvent device state eventChan ev = do
  let
    -- | Atomically modify one of the calls.
    callOperation :: CallId
                  -- ^ CallId to find in calls map
                  -> (Call -> Map.Map CallId Call -> Map.Map CallId Call)
                  -- ^ How to modify calls map if the call has been found
                  -> String
                  -- ^ Error message if no such call found
                  -> STM ()
    callOperation callId callMod err =
        modifyTVar state $
          \m ->
            case Map.lookup callId (_calls m) of
              Just call ->
                m & calls %~ (callMod call)
              Nothing ->
                error err
  now <- getCurrentTime
  atomically $ do
    case ev of
      -- New call
      Rs.DeliveredEvent{..} -> do
        let (dir, interloc) = if callingDevice == device
                              then (Out, calledDevice)
                              else (In, callingDevice)
            call = Call dir now [interloc] Nothing False
        modifyTVar state (calls %~ Map.insert callId call)
      Rs.EstablishedEvent{..} ->
        callOperation callId
        (\call -> Map.insert callId call{answered = Just now}) $
        "Established connection to an undelivered call"
      -- ConnectionCleared event arrives when line is put on HOLD too.
      -- A real call-ending ConnectionCleared is distinguished by its
      -- releasingDevice value.
      Rs.ConnectionClearedEvent{..} ->
        when (releasingDevice == device) $
        modifyTVar state $ (calls %~ at callId .~ Nothing)
      Rs.HeldEvent{..} ->
        callOperation callId
        (\call -> Map.insert callId call{held = True}) $
        "Held an undelivered call"
      Rs.RetrievedEvent{..} ->
        callOperation callId
        (\call -> Map.insert callId call{held = False}) $
        "Retrieved an undelivered call"
      -- Conferencing call A to call B yields ConferencedEvent with
      -- primaryCall=B and secondaryCall=A, while call A dies.
      Rs.ConferencedEvent prim sec -> do
        s <- readTVar state
        case (Map.lookup prim (_calls s), Map.lookup sec (_calls s)) of
          (Just oldCall, Just newCall) -> do
            modifyTVar state $ (calls %~ at prim .~ Nothing)
            callOperation sec
              (\call ->
                 Map.insert sec call{interlocutors = interlocutors oldCall ++
                                                     interlocutors newCall})
              "Conferenced an undelivered call"
          -- ConferencedEvent may also be produced after a new
          -- established call (not caused by an actual conference user
          -- request)
          _ -> return ()
      Rs.UnknownEvent -> return ()
    s <- readTVar state
    writeTChan eventChan $ Event ev s


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
        sendRequestSync (cstaHandle as) $
          Rq.MonitorStop
          { acceptedProtocol = protocolVersion as
          , monitorCrossRefID = monitorId ag
          }
        sendRequestSync (cstaHandle as) $
          Rq.ReleaseDeviceId{device = deviceId ag}
        killThread (actionThread ag)
        atomically $ modifyTVar' (agents as) (Map.delete aid)
        releaseAgentLock (aid, as)
        return ()


handleEvents :: AgentHandle -> (Event -> IO ()) -> IO ThreadId
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


getAgentState :: AgentHandle
              -> IO AgentState
getAgentState (aid, as) = do
  ags <- readTVarIO (agents as)
  case Map.lookup aid ags of
    Nothing -> throwIO $ UnknownAgent aid
    Just (Agent{..}) -> readTVarIO state
