{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.Avaya.Messages where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import           Text.Hamlet.XML
import           Text.XML

import           Network.Avaya.Types


d :: Name -> T.Text -> [Node] -> B.ByteString
d name namespace ns = renderLBS def $ Document (Prologue [] Nothing []) root []
  where
    root = Element name [("xmlns", namespace)] ns


getProtocolString ver =
    case ver of
      "3.0" -> "3.0"
      "3.1" -> "http://www.ecma-international.org/standards/ecma-323/csta/ed2/priv1"
      "4.0" -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv1"
      "4.1" -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv2"
      "4.2" -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv3"
      "5.2" -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv4"
      "6.1" -> "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv5"
      _     -> error "getProtocolString: no such version"


startAppSessionMessage :: Conf -> B.ByteString
startAppSessionMessage Conf { cUser = username, cUserPassword = password, cDelay = delay, cVersion = version, cDuration = duration } =
    d "StartApplicationSession" "http://www.ecma-international.org/standards/ecma-354/appl_session" [xml|
<applicationInfo>
    <applicationID>TestApp
    <applicationSpecificInfo>
        <ns1:SessionLoginInfo xsi:type="ns1:SessionLoginInfo" xmlns:ns1="http://www.avaya.com/csta" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <ns1:userName>#{username}
            <ns1:password>#{password}
            <ns1:sessionCleanupDelay>#{delay}
<requestedProtocolVersions>
    <protocolVersion>#{getProtocolString version}
<requestedSessionDuration>#{duration}
|]


resetApplicationSessionTimer :: T.Text -> B.ByteString
resetApplicationSessionTimer sessionId =
    d "ResetApplicationSessionTimer" "http://www.ecma-international.org/standards/ecma-354/appl_session" [xml|
<sessionID>#{sessionId}
<requestedSessionDuration>180
|]


getDeviceIdMessage :: Conf -> B.ByteString
getDeviceIdMessage Conf { cCallServerIp = ip, cExtension = extension } =
    d "GetDeviceId" "http://www.avaya.com/csta" [xml|
<switchName>#{ip}
<extension>#{extension}
|]


monitorStartMessage :: T.Text -> T.Text -> B.ByteString
monitorStartMessage protocol deviceId =
    d "MonitorStart" protocol [xml|
<monitorObject>
    <deviceObject typeOfNumber="other" mediaClass="notKnown">#{deviceId}
<requestedMonitorFilter>
    <physicalDeviceFeature>
        <displayUpdated>true
        <hookswitch>true
        <lampMode>true
        <ringerStatus>true
<extensions>
    <privateData>
        <private>
            <AvayaEvents>
                <invertFilter>true
|]

registerTerminalRequestMessage (Conf { cPassword = password }) deviceId =
    d "RegisterTerminalRequest" "http://www.avaya.com/csta" [xml|
<device typeOfNumber="other" mediaClass="notKnown">#{deviceId}
<loginInfo>
    <forceLogin>true
    <sharedControl>false
    <password>#{password}
    <mediaMode>NONE
    <dependencyMode>DEPENDENT
|]


setHookswitchStatusMessage protocol deviceId =
    d "SetHookswitchStatus" protocol [xml|
<device typeOfNumber="other" mediaClass="notKnown">#{deviceId}
<hookswitch>0
<hookswitchOnhook>false
|]


buttonPressMessage :: T.Text -> T.Text -> T.Text -> B.ByteString
buttonPressMessage button protocol deviceId =
    d "ButtonPress" protocol [xml|
<device typeOfNumber="other" mediaClass="notKnown">#{deviceId}
<button>#{button}
|]


monitorStopMessage monitorId =
    d "MonitorStop" "http://www.avaya.com/csta" [xml|
<monitorCrossRefID>#{monitorId}
|]


releaseDeviceIdMessage deviceId =
    d "ReleaseDeviceId" "http://www.avaya.com/csta" [xml|
<device>#{deviceId}
|]


stopAppSession sessionId =
    d "StopApplicationSession" "http://www.ecma-international.org/standards/ecma-354/appl_session" [xml|
<sessionID>#{sessionId}
<sessionEndReason>
    <definedEndReason>normal
|]
