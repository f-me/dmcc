
{-# LANGUAGE QuasiQuotes #-}

module Avaya.Messages.Request where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Text.Hamlet.XML
import Text.XML


data Request
  = StartApplicationSession
    {applicationId :: Text
    ,requestedProtocolVersion :: ProtocolVersion
    ,userName :: Text
    ,password :: Text
    ,sessionCleanupDelay :: Int
    ,requestedSessionDuration :: Int
    }
  | StopApplicationSession
    {sessionID :: Text
    }
  | ResetApplicationSessionTimer
    {sessionId :: Text
    ,requestedSessionDuration :: Int
    }
  | GetDeviceId
    {switchName :: Text
    ,extension :: Text
    }
  | ReleaseDeviceId
    {device :: Text
    }
  | MonitorStart
    {acceptedProtocol :: Text
    ,deviceObject :: Text
    }
  | MonitorStop
    {acceptedProtocol :: Text
    ,monitorCrossRefID :: Text
    }
  | RegisterTerminalRequest
    {device :: Text
    ,password :: Text
    }
  | UnregisterTerminalRequest
    {device :: Text
    }
  | SetHookswitchStatus
    {acceptedProtocol :: Text
    ,device :: Text
    ,hookswitchOnhook :: Bool
    }
  deriving Show

data ProtocolVersion
  = V3_0 | V3_1
  | V4_0 | V4_1 | V4_2
  | V5_2
  | V6_1
  deriving Show

getProtocolString :: ProtocolVersion -> Text
getProtocolString ver
  = case ver of
    V3_0 -> "3.0"
    V3_1 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed2/priv1"
    V4_0 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv1"
    V4_1 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv2"
    V4_2 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv3"
    V5_2 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv4"
    V6_1 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv5"

nsAppSession :: Text
nsAppSession = "http://www.ecma-international.org/standards/ecma-354/appl_session"
nsXSI :: Text
nsXSI = "http://www.w3.org/2001/XMLSchema-instance"
nsCSTA :: Text
nsCSTA = "http://www.avaya.com/csta"

doc :: Name -> Text -> [Node] -> Document
doc name xmlns nodes = Document
  (Prologue [] Nothing [])
  (Element
    name
    (--Map.fromList
      [("xmlns",xmlns),("xmlns:xsi",nsXSI),("xmlns:csta",nsCSTA)])
    nodes)
  []


toXml :: Request -> L.ByteString
toXml rq = renderLBS def $ case rq of
  StartApplicationSession{..}
    -> doc "StartApplicationSession" nsAppSession [xml|
      <applicationInfo>
        <applicationID>#{applicationId}
        <applicationSpecificInfo>
          <csta:SessionLoginInfo xsi:type="csta:SessionLoginInfo">
            <csta:userName>#{userName}
            <csta:password>#{password}
            <csta:sessionCleanupDelay>#{T.pack $ show sessionCleanupDelay}
      <requestedProtocolVersions>
        <protocolVersion>#{getProtocolString requestedProtocolVersion}
      <requestedSessionDuration>#{T.pack $ show requestedSessionDuration}
      |]

  StopApplicationSession{..}
    -> doc "StopApplicationSession" nsAppSession [xml|
      <sessionID>#{sessionID}
      <sessionEndReason>
        <definedEndReason>normal
      |]

  ResetApplicationSessionTimer{..}
    -> doc "ResetApplicationSessionTimer" nsAppSession [xml|
      <sessionID>#{sessionId}
      <requestedSessionDuration>#{T.pack $ show requestedSessionDuration}
      |]

  GetDeviceId{..}
    -> doc "GetDeviceId" nsCSTA [xml|
      <switchName>#{switchName}
      <extension>#{extension}
      |]

  ReleaseDeviceId{..}
    -> doc "ReleaseDeviceId" nsCSTA [xml|
      <device>#{device}
      |]

  MonitorStart{..}
    -> doc "MonitorStart" acceptedProtocol [xml|
      <monitorObject>
        <deviceObject typeOfNumber="other" mediaClass="notKnown">#{deviceObject}
      <requestedMonitorFilter>
        <callcontrol>
          <bridged>true</bridged>
          <callCleared>true</callCleared>
          <conferenced>true</conferenced>
          <connectionCleared>true</connectionCleared>
          <delivered>true</delivered>
          <digitsDialed>true</digitsDialed>
          <diverted>true</diverted>
          <established>true</established>
          <failed>true</failed>
          <held>true</held>
          <offered>true</offered>
          <originated>true</originated>
          <queued>true</queued>
          <retrieved>true</retrieved>
          <serviceInitiated>true</serviceInitiated>
          <transferred>true</transferred>
        <physicalDeviceFeature>
          <buttonInformation>true</buttonInformation>
          <buttonPress>true</buttonPress>
          <displayUpdated>true</displayUpdated>
          <hookswitch>true</hookswitch>
          <microphoneGain>true</microphoneGain>
          <microphoneMute>true</microphoneMute>
          <ringerStatus>true</ringerStatus>
        <logicalDeviceFeature>
          <agentBusy>true</agentBusy>
          <agentLoggedOn>true</agentLoggedOn>
          <agentLoggedOff>true</agentLoggedOff>
          <agentNotReady>true</agentNotReady>
          <agentReady>true</agentReady>
          <agentWorkingAfterCall>true</agentWorkingAfterCall>
          <autoAnswer>true</autoAnswer>
          <autoWorkMode>true</autoWorkMode>
          <callBack>true</callBack>
          <callBackMessage>true</callBackMessage>
          <callerIDStatus>true</callerIDStatus>
          <doNotDisturb>true</doNotDisturb>
          <forwarding>true</forwarding>
          <routeingMode>true</routeingMode>
      <extensions>
        <privateData>
          <private>
            <AvayaEvents>
              <invertFilter>false
      |]

  MonitorStop{..}
    -> doc "MonitorStop" acceptedProtocol [xml|
      <monitorCrossRefID>#{monitorCrossRefID}
      |]

  RegisterTerminalRequest{..}
    -> doc "RegisterTerminalRequest" nsCSTA [xml|
      <device typeOfNumber="other" mediaClass="notKnown">#{device}
      <loginInfo>
          <forceLogin>true
          <sharedControl>false
          <password>#{password}
          <mediaMode>NONE
          <dependencyMode>DEPENDENT
      |]

  UnregisterTerminalRequest{..}
    -> doc "UnregisterTerminalRequest" nsCSTA [xml|
      <device>#{device}
      |]

  SetHookswitchStatus{..}
    -> let hook = if hookswitchOnhook then "true" else "false" 
    in doc "SetHookswitchStatus" acceptedProtocol [xml|
      <device typeOfNumber="other" mediaClass="notKnown">#{device}
      <hookswitch>0000
      <hookswitchOnhook>#{hook}
      |]
