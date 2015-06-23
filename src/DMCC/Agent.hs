{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Agents in a DMCC API session enable call control over devices.

-}

module DMCC.Agent

where

import           Control.Applicative
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

import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client (httpNoBody, method, requestBody)

import           DMCC.Session
import           DMCC.Types
import qualified DMCC.XML.Request as Rq
import qualified DMCC.XML.Response as Rs


-- | Actions performable by a controlled agent.
data Action = MakeCall{number :: Extension}
            | AnswerCall{callId :: CallId}
            | EndCall{callId :: CallId}
            | HoldCall{callId :: CallId}
            | RetrieveCall{callId :: CallId}
            | ConferenceCall{activeCall :: CallId, heldCall :: CallId}
            | TransferCall{activeCall :: CallId, heldCall :: CallId}
            | SendDigits{callId :: CallId, digits :: Text}
            | SetState{newState :: SettableAgentState}
            deriving Show


$(deriveJSON
  defaultOptions{sumEncoding = defaultTaggedObject{tagFieldName="action"}}
  ''Action)


data AgentSnapshot = AgentSnapshot
    { _calls :: Map.Map CallId Call
    , _state :: (Maybe AgentState, Text)
    -- ^ Last recognized agent state along with reason code.
    }
    deriving Show


instance ToJSON AgentSnapshot where
  toJSON s =
    object [ "calls" A..= (Map.mapKeys (\(CallId t) -> t) $ _calls s)
           , "state" A..= _state s
           ]


instance FromJSON AgentSnapshot where
  parseJSON (Object o) =
    AgentSnapshot
    <$> ((Map.mapKeys CallId) `liftM` (o .: "calls"))
    <*> (o .: "state")
  parseJSON _ = fail "Could not parse AgentSnapshot from non-object"


$(makeLenses ''AgentSnapshot)


-- | Events/errors are published to external clients of the
-- agents and may be used by agent subscribers to provide information
-- to user.
data AgentEvent = TelephonyEvent
                  { dmccEvent :: Rs.Event
                  , newSnapshot :: AgentSnapshot}
                -- ^ A telephony-related event, along with an updated
                -- snapshot.
                | StateChange{newSnapshot :: AgentSnapshot}
                -- ^ Arrives when an agent state change has been
                -- observed.
                | TelephonyEventError{errorText :: String}
                -- ^ An error caused by a telephony-related event.
                | RequestError{errorText :: String}
                -- ^ An error caused by a request from this agent.
                deriving Show


$(deriveJSON defaultOptions ''AgentEvent)


-- | Web hook event.
data WHEvent = WHEvent
    { agentId :: AgentId
    , event :: AgentEvent
    }
    deriving Show


$(deriveJSON defaultOptions ''WHEvent)


-- | An agent controlled by a DMCC API session.
data Agent = Agent
    { deviceId :: DeviceId
    , monitorId :: Text
    , actionChan :: TChan Action
    -- ^ Actions performed by the agent.
    , actionThread :: ThreadId
    , rspChan :: TChan Rs.Response
    -- ^ Input chan for XML responses for this agent (including
    -- events).
    , rspThread :: ThreadId
    -- ^ Response reader.
    , stateThread :: ThreadId
    -- ^ Agent state poller.
    , eventChan :: TChan AgentEvent
    , snapshot :: TVar AgentSnapshot
    }


instance Show Agent where
  show (Agent (DeviceId d) m _ _ _ _ _ _ _) =
    "Agent{deviceId=" ++ unpack (original d) ++
    ", monitorId=" ++ unpack m ++
    "}"


newtype AgentHandle = AgentHandle (AgentId, Session)


instance Ord AgentHandle where
  compare (AgentHandle (aid1, _)) (AgentHandle (aid2, _)) = compare aid1 aid2


instance Eq AgentHandle where
  (AgentHandle (aid1, _)) == (AgentHandle (aid2, _)) = aid1 == aid2


instance Show AgentHandle where
  show (AgentHandle (aid, _)) = show aid


-- | Exceptions thrown by agent-related routines and threads.
data AgentError
  = DeviceError String
  | MonitoringError String
  | StatePollingError String
  | UnknownAgent AgentId
  deriving (Data, Typeable, Show)


instance (Exception AgentError)


-- | Command an agent to do something.
--
-- Due to lack of global locking of the agents map an agent may be
-- gone (released) by the time an action arrives to its actionChan.
-- This is by design to avoid congestion during action processing.
agentAction :: Action -> AgentHandle -> IO ()
agentAction cmd (AgentHandle (aid, as)) = do
  ags <- readTVarIO (agents as)
  case Map.lookup aid ags of
    Just (Agent{..}) -> atomically $ writeTChan actionChan cmd
    Nothing -> throwIO $ UnknownAgent aid


placeAgentLock :: AgentHandle -> STM ()
placeAgentLock (AgentHandle (aid, as)) =
  modifyTVar' (agentLocks as) (Set.insert aid)


releaseAgentLock :: AgentHandle -> IO ()
releaseAgentLock (AgentHandle (aid, as)) =
  atomically $ modifyTVar' (agentLocks as) (Set.delete aid)


-- | Enable an active agent to be monitored and controlled through
-- DMCC API. If the agent has already been registered, return the old
-- entry (it's safe to call this function with the same arguments
-- multiple times).
controlAgent :: SwitchName
             -> Extension
             -> Session
             -> IO (Either AgentError AgentHandle)
controlAgent switch ext as = do
  let aid = AgentId (switch, ext)
      ah  = AgentHandle (aid, as)
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
          False -> placeAgentLock ah >> return Nothing

  case prev of
    Just a -> return $ Right $ AgentHandle (a, as)
    Nothing -> try $ flip onException (releaseAgentLock ah) $
      do
        -- Get Avaya info for this agent (note that requests are not
        -- attached to the agent (Nothing) as it has not been inserted
        -- in the agent map of the session yet).
        gdiRsp <-
          sendRequestSync (dmccHandle as) Nothing $
          Rq.GetDeviceId
          { switchName = switch
          , extension = ext
          }

        device <-
          case gdiRsp of
            Just (Rs.GetDeviceIdResponse device) ->
              return device
            Just (Rs.CSTAErrorCodeResponse errorCode) ->
              throwIO $ DeviceError $ unpack errorCode
            _ ->
              throwIO $ DeviceError "Bad GetDeviceId response"

        msrRsp <-
          sendRequestSync (dmccHandle as) Nothing $
          Rq.MonitorStart
          { acceptedProtocol = protocolVersion as
          , monitorRq = Rq.Device device
          }

        monitorCrossRefID <-
          case msrRsp of
            Just (Rs.MonitorStartResponse monitorCrossRefID) ->
              return monitorCrossRefID
            Just (Rs.CSTAErrorCodeResponse errorCode) ->
              throwIO $ MonitoringError $ unpack errorCode
            _ ->
              throwIO $ DeviceError "Bad MonitorStart response"

        -- Setup action and event processing for this agent
        snapshot <- newTVarIO $ AgentSnapshot Map.empty (Nothing, "")

        actionChan <- newTChanIO
        actionThread <-
          forkIO $ forever $
          (atomically $ readTChan actionChan) >>=
          processAgentAction aid device snapshot as

        eventChan <- newBroadcastTChanIO

        rspChan <- newTChanIO
        rspThread <-
          forkIO $ forever $
          (atomically $ readTChan rspChan) >>=
          processAgentEvent aid device snapshot eventChan as

        -- As of DMCC 6.2.x, agent state change events are not
        -- reported by DMCC (see
        -- https://www.devconnectprogram.com/forums/posts/list/18511.page).
        -- We use polling to inform our API clients about update in
        -- the agent state.

        -- Find out initial agent state
        gsRsp' <- sendRequestSync (dmccHandle as) (Just aid) $
              Rq.GetAgentState
              { device = device
              , acceptedProtocol = protocolVersion as
              }
        case gsRsp' of
          Just (Rs.GetAgentStateResponse{..}) ->
            atomically $
            modifyTVar' snapshot (state .~ (agentState, reasonCode))
          Just (Rs.CSTAErrorCodeResponse errorCode) ->
            throwIO $ StatePollingError $ unpack errorCode
          _ ->
            throwIO $ DeviceError "Bad GetAgentState response"
        -- State polling thread
        stateThread <-
          forkIO $ forever $ do
            gsRsp <- sendRequestSync (dmccHandle as) (Just aid) $
              Rq.GetAgentState
              { device = device
              , acceptedProtocol = protocolVersion as
              }
            case gsRsp of
              Just (Rs.GetAgentStateResponse{..}) -> do
                ns <- atomically $ do
                  sn <- readTVar snapshot
                  case (_state sn /= (agentState, reasonCode)) of
                    False -> return Nothing
                    True -> do
                      modifyTVar' snapshot (state .~ (agentState, reasonCode))
                      readTVar snapshot >>= \newSnapshot -> do
                        writeTChan eventChan $ StateChange newSnapshot
                        return $ Just newSnapshot
                case (ns, webHook as) of
                  (Just ns', Just connData) ->
                    sendWH connData aid (StateChange ns')
                  _ -> return ()
              -- Ignore state errors
              _ -> return ()
            threadDelay $
              (statePollingDelay $ sessionOptions $ dmccHandle as) * 1000000

        let ag = Agent
                 device
                 monitorCrossRefID
                 actionChan
                 actionThread
                 rspChan
                 rspThread
                 stateThread
                 eventChan
                 snapshot
        atomically $ modifyTVar' (agents as) (Map.insert aid ag)
        releaseAgentLock ah
        return ah


-- | Translate agent actions into actual DMCC API requests.
--
-- TODO Allow agents to control only own calls.
processAgentAction :: AgentId
                   -> DeviceId
                   -> TVar AgentSnapshot
                   -> Session
                   -> Action
                   -> IO ()
processAgentAction aid@(AgentId (switch, _)) device snapshot as action =
  let
    arq = sendRequestAsync (dmccHandle as) (Just aid)
    simpleRequest rq arg =
      arq $ rq device arg (protocolVersion as)
    simpleRequest2 rq arg1 arg2 =
      arq $ rq device arg1 arg2 (protocolVersion as)
  in
  case action of
    MakeCall toNumber -> do
      Just rspDest <-
        sendRequestSync (dmccHandle as) (Just aid) $
        Rq.GetThirdPartyDeviceId
        -- Assume destination switch is the same as agent's
        { switchName = switch
        , extension = toNumber
        }
      Just mcr <- sendRequestSync (dmccHandle as) (Just aid) $
        Rq.MakeCall
        device
        (Rs.device rspDest)
        (protocolVersion as)
      case mcr of
        Rs.MakeCallResponse callId ucid -> do
          now <- getCurrentTime
          let call = Call Out ucid now [Rs.device rspDest] Nothing False False
          atomically $ modifyTVar' snapshot (calls %~ Map.insert callId call)
        _ -> return ()
    AnswerCall callId   -> simpleRequest Rq.AnswerCall callId
    HoldCall callId     -> simpleRequest Rq.HoldCall callId
    RetrieveCall callId -> simpleRequest Rq.RetrieveCall callId
    ConferenceCall activeCall heldCall ->
      simpleRequest2 Rq.ConferenceCall activeCall heldCall
    TransferCall activeCall heldCall ->
      simpleRequest2 Rq.TransferCall activeCall heldCall
    EndCall callId -> simpleRequest Rq.ClearConnection callId
    -- Synchronous to avoid missed digits if actions queue up
    SendDigits{..} ->
      void $
      sendRequestSync (dmccHandle as) (Just aid) $
      Rq.GenerateDigits digits device callId (protocolVersion as)
    SetState newState -> do
      cs <- atomically $ readTVar snapshot
      when (fst (_state cs) /= Just Busy) $
        simpleRequest Rq.SetAgentState newState


-- | Process DMCC API events/errors for this agent to change its snapshot
-- and broadcast events further.
processAgentEvent :: AgentId
                  -> DeviceId
                  -> TVar AgentSnapshot
                  -> TChan AgentEvent
                  -> Session
                  -> Rs.Response
                  -> IO ()
processAgentEvent aid device snapshot eventChan as rs = do
  let
    -- | Atomically modify one of the calls.
    callOperation :: CallId
                  -- ^ CallId to find in calls map
                  -> (Call -> Map.Map CallId Call -> Map.Map CallId Call)
                  -- ^ How to modify calls map if the call has been found
                  -> String
                  -- ^ Error message if no such call found
                  -> STM ()
    callOperation callId callMod err = do
      s <- readTVar snapshot
      case Map.lookup callId (_calls s) of
        Just call ->
          modifyTVar' snapshot $ \m -> m & calls %~ (callMod call)
        Nothing ->
          writeTChan eventChan $ TelephonyEventError err
  now <- getCurrentTime
  case rs of
    Rs.CSTAErrorCodeResponse err ->
      atomically $ writeTChan eventChan $ RequestError $ unpack err

    -- New outgoing call
    --
    -- MakeCallResponse handler should have already added the call
    -- information to agent state. If this is not the case (which
    -- occurs when multiple DMCC library sessions interact with the
    -- same AVAYA instance), we add the call after finding out its
    -- UCID.
    Rs.EventResponse _ ev@Rs.OriginatedEvent{..} -> do
      s <- readTVarIO snapshot
      updSnapshot' <-
        case Map.member callId (_calls s) of
          True -> return s
          False -> do
            Just gcldr <-
              sendRequestSync (dmccHandle as) (Just aid) $
              Rq.GetCallLinkageData device callId (protocolVersion as)
            case gcldr of
              Rs.GetCallLinkageDataResponse ucid ->
                atomically $ do
                  modifyTVar' snapshot (calls %~ Map.insert callId call)
                  readTVar snapshot
                  where
                    call = Call Out ucid now [calledDevice] Nothing False False
              _ -> do
                atomically $ writeTChan eventChan $
                  TelephonyEventError "Bad GetCallLinkageDataResponse"
                return s
      atomically $ writeTChan eventChan $ TelephonyEvent ev s
      case webHook as of
        Just connData -> sendWH connData aid (TelephonyEvent ev updSnapshot')
        _             -> return ()


    -- All other telephony events
    Rs.EventResponse _ ev -> do
      (updSnapshot, report) <- atomically $ do
        -- Return True if the event must be reported to agent event chan
        report <- case ev of
          Rs.OriginatedEvent{..} ->
            -- Should have already been handled in a branch above
            return True
          -- New call
          Rs.DeliveredEvent{..} -> do
            s <- readTVar snapshot
            case Map.member callId $ _calls s of
              -- DeliveredEvent arrives after OriginatedEvent for
              -- outgoing calls too, but we keep the original call
              -- information.
              True -> return False
              False -> do
                modifyTVar' snapshot (calls %~ Map.insert callId call)
                return True
            where
              (dir, interloc) = if callingDevice == device
                                then (Out, calledDevice)
                                else (In distributingVdn, callingDevice)
              call = Call dir ucid now [interloc] Nothing False False
          Rs.DivertedEvent{..} -> do
            modifyTVar' snapshot $ (calls %~ at callId .~ Nothing)
            return True
          Rs.EstablishedEvent{..} -> do
            callOperation callId
              (\call -> Map.insert callId call{answered = Just now}) $
              "Established connection to an undelivered call"
            return True
          Rs.FailedEvent{..} -> do
            callOperation callId
              (\call -> Map.insert callId call{failed = True}) $
              "Failed an unknown call"
            return True
          -- ConnectionCleared event arrives when line is put on HOLD too.
          -- A real call-ending ConnectionCleared is distinguished by its
          -- releasingDevice value.
          Rs.ConnectionClearedEvent{..} -> do
            let really = releasingDevice == device
            when really $ modifyTVar' snapshot $ (calls %~ at callId .~ Nothing)
            return really
          Rs.HeldEvent{..} -> do
            callOperation callId
              (\call -> Map.insert callId call{held = True}) $
              "Held an undelivered call"
            return True
          Rs.RetrievedEvent{..} -> do
            callOperation callId
              (\call -> Map.insert callId call{held = False}) $
              "Retrieved an undelivered call"
            return True
          -- Conferencing call A to call B yields ConferencedEvent with
          -- primaryCall=B and secondaryCall=A, while call A dies.
          Rs.ConferencedEvent prim sec -> do
            s <- readTVar snapshot
            case (Map.lookup prim (_calls s), Map.lookup sec (_calls s)) of
              (Just oldCall, Just newCall) -> do
                modifyTVar' snapshot $ (calls %~ at prim .~ Nothing)
                callOperation sec
                  (\call ->
                     Map.insert sec
                     call{interlocutors = interlocutors oldCall ++
                                          interlocutors newCall})
                  "Conferenced an undelivered call"
                return True
              -- ConferencedEvent may also be produced after a new
              -- established call but not caused by an actual conference
              -- user request (recorder single-stepping in).
              _ -> return False
          Rs.TransferedEvent prim sec -> do
            modifyTVar' snapshot $ (calls %~ at prim .~ Nothing)
            modifyTVar' snapshot $ (calls %~ at sec .~ Nothing)
            return True
          Rs.UnknownEvent -> return False
        s <- readTVar snapshot
        when report $ writeTChan eventChan $ TelephonyEvent ev s
        return (s, report)
      -- Call webhook if necessary
      case (webHook as, report) of
        (Just connData, True) ->
          sendWH connData aid (TelephonyEvent ev updSnapshot)
        _ -> return ()
    -- All other responses cannot arrive to an agent
    _ -> return ()


-- | Send agent event data to a web hook endpoint, ignoring possible
-- exceptions.
sendWH :: (HTTP.Request, HTTP.Manager)
       -> AgentId
       -> AgentEvent
       -> IO ()
sendWH (req, mgr) aid payload = do
  void $ httpNoBody req{requestBody = rqBody, method = "POST"} mgr
    where
      rqBody = HTTP.RequestBodyLBS $ A.encode $ WHEvent aid payload


-- | Forget about an agent, releasing his device and monitors.
releaseAgent :: AgentHandle
             -> IO ()
releaseAgent ah@(AgentHandle (aid, as)) = do
  prev <- atomically $ do
    locks <- readTVar (agentLocks as)
    case Set.member aid locks of
      True -> retry
      False -> do
        ags <- readTVar (agents as)
        case Map.lookup aid ags of
          Just ag -> placeAgentLock ah >> (return $ Just ag)
          Nothing -> return Nothing

  case prev of
    Nothing -> throwIO $ UnknownAgent aid
    Just ag ->
      handle (\e ->
                releaseAgentLock ah >>
                (throwIO (e :: IOException))) $
      do
        sendRequestSync (dmccHandle as) (Just aid) $
          Rq.MonitorStop
          { acceptedProtocol = protocolVersion as
          , monitorCrossRefID = monitorId ag
          }
        sendRequestSync (dmccHandle as) (Just aid) $
          Rq.ReleaseDeviceId{device = deviceId ag}
        killThread (actionThread ag)
        killThread (rspThread ag)
        killThread (stateThread ag)
        atomically $ modifyTVar' (agents as) (Map.delete aid)
        releaseAgentLock ah
        return ()


-- | Attach an event handler to an agent. Exceptions are not handled.
handleEvents :: AgentHandle -> (AgentEvent -> IO ()) -> IO ThreadId
handleEvents (AgentHandle (aid, as)) handler = do
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


getAgentSnapshot :: AgentHandle
                 -> IO AgentSnapshot
getAgentSnapshot (AgentHandle (aid, as)) = do
  ags <- readTVarIO (agents as)
  case Map.lookup aid ags of
    Nothing -> throwIO $ UnknownAgent aid
    Just (Agent{..}) -> readTVarIO snapshot
