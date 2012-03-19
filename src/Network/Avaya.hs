{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.Avaya where

import           Prelude hiding (catch, getContents)
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Word
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString.Lazy

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Data.Text.Lazy.Encoding
import           Text.Hamlet.XML
import           Text.XML
import           Text.XML.Cursor


host         = "192.168.20.5"  -- AES IP address
port         = "4721"          -- default AES ports are 4721 and 4722
username     = "dev"
password     = "1qaz@WSX"
callServerIp = "192.168.20.2"  -- ?
extension    = "4750"          -- ?


-- | Four specific XML messages must be constructed by the application
-- and sent to the connector server in order to get started:
--
-- 1. StartApplicationSession;
-- 2. GetDeviceId;
-- 3. MonitorStart;
-- 4. RegisterTerminal - required only for device/media control
-- applications, and not for applications using call control
-- exclusively.
test = withSocketsDo $ do
    sock <- connectTo host port

    -- Establish an application session.
    sendRequest sock startAppSession "0001"
    res <- recvResponse sock
    putStrLn $ "* startAppSessionResponse:\n" ++ B.unpack res

    let sessionId = parseSessionId res

    when (isPosResponse res) $ do
        -- Get device identifier.
        sendRequest sock getDeviceIdMessage "0002"
        res <- recvResponse sock
        putStrLn $ "* getDeviceIdResponseMessage:\n" ++ B.unpack res

        let devId = parseDeviceId res

        when (not $ T.null devId) $ do
            -- Request notification of events.
            sendRequest sock (monitorStartMessage devId) "0003"
            res <- recvResponse sock
            putStrLn $ "* monitorStartResponseMessage:\n" ++ B.unpack res

            let monId = parseMonitorId res

            telephonyLogic
            cleanUp sock monId devId sessionId


telephonyLogic = putStrLn "No telephony logic implemented in this example application."


cleanUp sock monId devId sessionId = do
    -- Stop monitoring the events.
    sendRequest sock (monitorStopMessage monId) "0004"
    res <- recvResponse sock
    putStrLn $ "* monitorStopResponseMessage:\n" ++ B.unpack res

    -- Release the device identifier.
    sendRequest sock (releaseDeviceIdMessage devId) "0005"
    res <- recvResponse sock
    putStrLn $ "* releaseDeviceResponseMessage:\n" ++ B.unpack res

    -- Close the app session.
    sendRequest sock (stopAppSession sessionId) "0006"
    res <- recvResponse sock
    putStrLn $ "* stopAppSessionResponseMessage:\n" ++ B.unpack res


------------------------------------------------------------------------------
sendRequest :: Socket -> B.ByteString -> B.ByteString -> IO ()
sendRequest sock request invokeId = do
    -- hPutStr h . B.unpack . runPut $ putHeader request invokeId  -- FIXME: maybe Network.Socket.ByteString?
    send sock . runPut $ putHeader request invokeId  -- FIXME: send function is Unix only
    return ()


recvResponse :: Socket -> IO B.ByteString
recvResponse sock = do
    --    B.concat . (map B.pack) <$> doread id
    header <- runGet getHeader <$> recv sock 8
    recv sock . fromIntegral $ cLength header


------------------------------------------------------------------------------
-- | All XML messages must have prepended the appropriate eight byte
-- header as specified by CSTA.
-- |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  | ...        |
-- |  Version  |   Length  |       InvokeID        | XML Message Body |
data CstaHeader = CstaHeader {
      cVersion  :: Word16
    , cLength   :: Word16
    , cInvokeId :: Word32  -- ^ request and response XML messages must use Invoke IDs between 0 and 9998
    }


putHeader :: B.ByteString -> B.ByteString -> Put
putHeader request invokeId = do
    putWord16be 0
    putWord16be . fromIntegral $ 8 + B.length request
    putLazyByteString invokeId
    putLazyByteString request


getHeader :: Get CstaHeader
getHeader = do
    version  <- getWord16be
    length   <- getWord16be
    invokeId <- getWord32be
    return $ CstaHeader version (length - 8) invokeId


------------------------------------------------------------------------------
parseResponse :: B.ByteString -> Document
parseResponse = parseLBS_ def  -- FIXME: maybe we should use parseLBS that returns Either SomeException Document?


isPosResponse res =
    bool $ fromDocument doc $| laxElement "StartApplicationSessionPosResponse"
  where
    doc = parseResponse res

parseSessionId  = sessionId . parseResponse
parseDeviceId   = deviceId  . parseResponse
parseMonitorId  = monitorId . parseResponse


sessionId doc = el doc "sessionID"
deviceId  doc = el doc "device"
monitorId doc = el doc "monitorCrossRefID"


el doc name =
    head $ fromDocument doc $/  laxElement name  -- FIXME: head on empty list!
                            &// content

------------------------------------------------------------------------------
-- Each release adds functionality to the previous releases
-- 3.0 "3.0"
-- 3.1 "http://www.ecma-international.org/standards/ecma-323/csta/ed2/priv1"
-- 4.0 "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv1"
-- 4.1 "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv2"
-- 4.2 "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv3"
-- 5.2 "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv4"
-- 6.1 "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv5"
protocol = "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv3"
delay    = "5"

-- Amount of time in seconds that a session will remain active.
duration = "180"


startAppSession =
    d "StartApplicationSession" "http://www.ecma-international.org/standards/ecma-354/appl_session" [xml|
<applicationInfo>
    <applicationID>TestApp
    <applicationSpecificInfo>
        <ns1:SessionLoginInfo xsi:type="ns1:SessionLoginInfo" xmlns:ns1="http://www.avaya.com/csta" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <ns1:userName>#{username}
            <ns1:password>#{password}
            <ns1:sessionCleanupDelay>#{delay}
<requestedProtocolVersions>
    <protocolVersion>#{protocol}
<requestedSessionDuration>#{duration}
|]


resetApplicationSessionTimer =
    d "ResetApplicationSessionTimer" "http://www.ecma-international.org/standards/ecma-354/appl_session" [xml|
<sessionID>469421A9364D46D6444524AE41BEAD72-0
<requestedSessionDuration>180
|]


getDeviceIdMessage =
    d "GetDeviceId" "http://www.avaya.com/csta" [xml|
<switchName>#{callServerIp}
<extension>#{extension}
|]


monitorStartMessage deviceId =
    d "MonitorStart" protocol [xml|
<monitorObject>
    <deviceObject typeOfNumber="other" mediaClass="notKnown">#{deviceId}
<requestedMonitorFilter>
    <physicalDeviceFeature>
        <displayUpdated>true
        <hookswitch>false
        <lampMode>true
        <ringerStatus>false
<extensions>
    <privateData>
        <private>
            <AvayaEvents>
                <invertFilter>true
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


------------------------------------------------------------------------------
d name namespace ns = renderLBS def $ Document (Prologue [] Nothing []) root []
  where
    root = Element name [("xmlns", namespace)] ns


------------------------------------------------------------------------------
connectTo :: HostName -> ServiceName -> IO Socket
connectTo host port = do
    addrs <- getAddrInfo Nothing (Just host) (Just port)
    firstSuccessful $ map tryToConnect addrs
  where
    tryToConnect addr =
        bracketOnError
          (socket (addrFamily addr) Stream defaultProtocol)
          (sClose)
          (\sock -> do
             connect sock (addrAddress addr)
             return sock)


catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch


firstSuccessful :: [IO a] -> IO a
firstSuccessful [] = error "firstSuccessful: empty list"
firstSuccessful (p:ps) = catchIO p $ \e ->
    case ps of
        [] -> throw e
        _  -> firstSuccessful ps
