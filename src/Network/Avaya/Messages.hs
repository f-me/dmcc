{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.Avaya.Messages where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import           Text.Hamlet.XML
import           Text.XML

import           Network.Avaya.Types


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
    serialize $ 
      StartApplicationSession 
        (ApplicationInfo
          (XsdString "TestApp")
          (ApplicationSpecificInfo $ ANYSchemaType $
            SessionLoginInfo 
              (XsdString username)
              (XsdString password)
              delay
              Nothing))
        (RequestedProtocolVersions [getProtocolString version])
        (Just duration)

resetApplicationSessionTimer sessionId =
    serialize $
      ResetApplicationSessionTimer
        sessionId
        180
        []

getDeviceIdMessage :: Conf -> B.ByteString
getDeviceIdMessage Conf { cCallServerIp = ip, cExtension = extension } =
    serialize $
      GetDeviceId
        (XsdString ip)
        [extension]

monitorStartMessage :: T.Text -> T.Text -> B.ByteString
monitorStartMessage protocol deviceId =
    serialize $
      MonitorStart
        (MonitorObject
          deviceId)
        (RequestedMonitorFilter
          (PhysicalDeviceFeature
            true
            true
            true
            true))
-- FIXME       (Extensions )
{-
<extensions>
    <privateData>
        <private>
            <AvayaEvents>
                <invertFilter>true
-}

registerTerminalRequestMessage (Conf { cPassword = password }) deviceId =
    serialize $
        RegisterTerminalRequest
          deviceId
          (LoginInfo
            true
            false
            (XsdString password)
            NONE
            DEPENDENT)

setHookswitchStatusMessage onHook protocol deviceId =
    serialize $
      SetHookswitchStatus
      deviceId
      (HookswitchID 0000)
      onHook
      Nothing

buttonPressMessage button deviceId =
    serialize $
      ButtonPress
        deviceId
        button
        Nothing

monitorStopMessage monitorId =
    serialize $
      MonitorStop
        monitorId
        Nothing

releaseDeviceIdMessage deviceId =
    serialize $
      ReleaseDeviceId deviceId

stopAppSession sessionId =
    serialize $
      StopApplicationSession 
        sessionId
        (SessionEndReason Normal)
