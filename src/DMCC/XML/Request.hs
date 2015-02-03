{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

{-| Low-level XML API requests. -}

module DMCC.XML.Request

where

import qualified Data.ByteString.Lazy as L
import           Data.CaseInsensitive
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Hamlet.XML
import           Text.XML

import           DMCC.Types


data Request
  = StartApplicationSession
    { applicationId :: Text
    , requestedProtocolVersion :: ProtocolVersion
    , userName :: Text
    , password :: Text
    , sessionCleanupDelay :: Int
    , requestedSessionDuration :: Int
    }
  | StopApplicationSession
    { sessionID :: Text
    }
  | ResetApplicationSessionTimer
    { sessionId :: Text
    , requestedSessionDuration :: Int
    }
  | GetDeviceId
    { switchName :: SwitchName
    , extension :: Extension
    }
  | GetThirdPartyDeviceId
    { switchName :: SwitchName
    , extension :: Extension
    }
  | MakeCall
    { callingDevice :: DeviceId
    , calledDirectoryNumber :: DeviceId
    , acceptedProtocol :: Text
    }
  | AnswerCall
    { deviceId :: DeviceId
    , callId :: CallId
    , acceptedProtocol :: Text
    }
  | HoldCall
    { deviceId :: DeviceId
    , callId :: CallId
    , acceptedProtocol :: Text
    }
  | RetrieveCall
    { deviceId :: DeviceId
    , callId :: CallId
    , acceptedProtocol :: Text
    }
  | GenerateDigits
    { charactersToSend :: Text
    , deviceId :: DeviceId
    , callId :: CallId
    , acceptedProtocol :: Text
    }
  | ConferenceCall
    { deviceId :: DeviceId
    , activeCall :: CallId
    , heldCall :: CallId
    , acceptedProtocol :: Text
    }
  | TransferCall
    { deviceId :: DeviceId
    , activeCall :: CallId
    , heldCall :: CallId
    , acceptedProtocol :: Text
    }
  | ClearConnection
    { deviceId :: DeviceId
    , callId :: CallId
    , acceptedProtocol :: Text
    }
  | ReleaseDeviceId
    { device :: DeviceId
    }
  | MonitorStart
    { acceptedProtocol :: Text
    , deviceObject :: DeviceId
    }
  | MonitorStop
    { acceptedProtocol :: Text
    , monitorCrossRefID :: Text
    }
  deriving Show


data ProtocolVersion = DMCC_4_2 | DMCC_6_1 | DMCC_6_2 | DMCC_6_3
                       deriving Show


getProtocolString :: ProtocolVersion -> Text
getProtocolString ver
  = case ver of
    DMCC_4_2 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv3"
    DMCC_6_1 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv5"
    DMCC_6_2 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv6"
    DMCC_6_3 -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv7"


nsAppSession :: Text
nsAppSession = "http://www.ecma-international.org/standards/ecma-354/appl_session"


nsXSI :: Text
nsXSI = "http://www.w3.org/2001/XMLSchema-instance"


nsACX :: Text
nsACX = "http://www.avaya.com/csta"


doc :: Name -> Text -> [Node] -> Document
doc name xmlns nodes = Document
  (Prologue [] Nothing [])
  (Element
    name
    (Map.fromList
      [ ("xmlns", xmlns)
      , ("xmlns:xsi", nsXSI)
      , ("xmlns:acx", nsACX)
      ])
    nodes)
  []


class ToText a where
  toText :: a -> Text


instance ToText (CI Text) where
  toText t = toText $ original t


instance ToText Text where
  toText t = t


instance ToText Extension where
  toText (Extension e) = T.pack $ show e


deriving instance ToText DeviceId


deriving instance ToText SwitchName


deriving instance ToText CallId


toXml :: Request -> L.ByteString
toXml rq = renderLBS def $ case rq of
  StartApplicationSession{..} ->
    doc "StartApplicationSession" nsAppSession
    [xml|
      <applicationInfo>
        <applicationID>#{applicationId}
        <applicationSpecificInfo>
          <acx:SessionLoginInfo xsi:type="acx:SessionLoginInfo">
            <acx:userName>#{userName}
            <acx:password>#{password}
            <acx:sessionCleanupDelay>#{T.pack $ show sessionCleanupDelay}
      <requestedProtocolVersions>
        <protocolVersion>#{getProtocolString requestedProtocolVersion}
      <requestedSessionDuration>#{T.pack $ show requestedSessionDuration}
    |]

  StopApplicationSession{..} ->
    doc "StopApplicationSession" nsAppSession
    [xml|
      <sessionID>#{sessionID}
      <sessionEndReason>
        <definedEndReason>normal
    |]

  ResetApplicationSessionTimer{..} ->
    doc "ResetApplicationSessionTimer" nsAppSession
    [xml|
      <sessionID>#{sessionId}
      <requestedSessionDuration>#{T.pack $ show requestedSessionDuration}
    |]

  GetDeviceId{..} ->
    doc "GetDeviceId" nsACX
    [xml|
      <switchName>#{toText switchName}
      <extension>#{toText extension}
      <controllableByOtherSessions>true
    |]

  GetThirdPartyDeviceId{..} ->
    doc "GetThirdPartyDeviceId" nsACX
    [xml|
      <switchName>#{toText switchName}
      <extension>#{toText extension}
    |]

  MakeCall{..} ->
    doc "MakeCall" acceptedProtocol
    [xml|
      <callingDevice>#{toText callingDevice}
      <calledDirectoryNumber>#{toText calledDirectoryNumber}
    |]

  AnswerCall{..} ->
    doc "AnswerCall" acceptedProtocol
    [xml|
      <callToBeAnswered>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText callId}
    |]

  HoldCall{..} ->
    doc "HoldCall" acceptedProtocol
    [xml|
      <callToBeHeld>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText callId}
    |]

  RetrieveCall{..} ->
    doc "RetrieveCall" acceptedProtocol
    [xml|
      <callToBeRetrieved>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText callId}
    |]

  GenerateDigits{..} ->
    doc "GenerateDigits" acceptedProtocol
    [xml|
      <connectionToSendDigits>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText callId}
      <charactersToSend>#{charactersToSend}
    |]

  ConferenceCall{..} ->
    doc "ConferenceCall" acceptedProtocol
    [xml|
      <heldCall>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText heldCall}
      <activeCall>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText activeCall}
    |]

  TransferCall{..} ->
    doc "TransferCall" acceptedProtocol
    [xml|
      <heldCall>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText heldCall}
      <activeCall>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText activeCall}
    |]

  ClearConnection{..} ->
    doc "ClearConnection" acceptedProtocol
    [xml|
      <connectionToBeCleared>
        <deviceID typeOfNumber="other" mediaClass="notKnown">#{toText deviceId}
        <callID>#{toText callId}
    |]

  ReleaseDeviceId{..} ->
    doc "ReleaseDeviceId" nsACX
    [xml|
      <device>#{toText device}
    |]

  MonitorStart{..} ->
    doc "MonitorStart" acceptedProtocol
    [xml|
      <monitorObject>
        <deviceObject typeOfNumber="other" mediaClass="notKnown">
          #{toText deviceObject}
      <requestedMonitorFilter>
        <callcontrol>
          <connectionCleared>true
          <conferenced>true
          <delivered>true
          <established>true
          <held>true
          <retrieved>true
          <transferred>true
      <extensions>
        <privateData>
          <private>
            <AvayaEvents>
              <invertFilter>true
    |]

  MonitorStop{..} ->
    doc "MonitorStop" acceptedProtocol
    [xml|
      <monitorCrossRefID>#{monitorCrossRefID}
    |]
