{-# LANGUAGE OverloadedStrings #-}

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
import           Text.XML
import           Text.XML.Cursor


host         = "192.168.20.5"  -- AES IP address
port         = "4721"          -- default AES ports are 4721 and 4722
username     = "user"
password     = "1234"
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
    sendRequest sock startAppSession 1
    res <- recvResponse sock
    putStrLn $ "* startAppSessionResponse:\n" ++ B.unpack res

    let sessionId = encodeUtf8 . L.fromStrict $ parseSessionId res

    when (isPosResponse $ parseResponse res) $ do
        -- Get device identifier.
        sendRequest sock getDeviceIdMessage 2
        res <- recvResponse sock
        putStrLn $ "* getDeviceIdResponseMessage:\n" ++ B.unpack res

        let devId = encodeUtf8 . L.fromStrict $ parseDeviceId res

        when (not $ B.null devId) $ do
            -- Request notification of events.
            sendRequest sock (monitorStartMessage devId) 3
            res <- recvResponse sock
            putStrLn $ "* monitorStartResponseMessage:\n" ++ B.unpack res

            let monId = encodeUtf8 . L.fromStrict $ parseMonitorId res

            telephonyLogic
            cleanUp monId sessionId


telephonyLogic = putStrLn "No telephony logic implemented in this example application."


cleanUp monId sessionId = do
    -- Stop monitoring the events.
    -- Close the app session.
    return ()

------------------------------------------------------------------------------
sendRequest :: Socket -> B.ByteString -> Word32 -> IO ()
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


putHeader :: B.ByteString -> Word32 -> Put
putHeader request invokeId = do
    putWord16be 0
    putWord16be . fromIntegral $ 8 + B.length request
    putWord32be invokeId
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


isPosResponse :: Document -> Bool
isPosResponse doc =
    bool $ fromDocument doc $| laxElement "StartApplicationSessionPosResponse"


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
protocol = "http://www.ecma-international.org/standards/ecma-323/csta/ed3/priv4"
delay    = "5"

-- Amount of time in seconds that a session will remain active.
duration = "180"


startAppSession =
    B.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<StartApplicationSession xmlns=\"http://www.ecma-international.org/standards/ecma-354/appl_session\">"
      ,     "<applicationInfo>"
      ,         "<applicationID>TestApp</applicationID>"
      ,         "<applicationSpecificInfo>"
      ,             "<ns1:SessionLoginInfo xsi:type=\"ns1:SessionLoginInfo\" xmlns:ns1=\"http://www.avaya.com/csta\""
      ,               " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"
      ,                   "<ns1:userName>", username, "</ns1:userName>"
      ,                   "<ns1:password>", password, "</ns1:password>"
      ,                   "<ns1:sessionCleanupDelay>", delay, "</ns1:sessionCleanupDelay>"
      ,             "</ns1:SessionLoginInfo>"
      ,         "</applicationSpecificInfo>"
      ,     "</applicationInfo>"
      ,     "<requestedProtocolVersions>"
      ,         "<protocolVersion>", protocol, "</protocolVersion>"
      ,     "</requestedProtocolVersions>"
      ,     "<requestedSessionDuration>", duration, "</requestedSessionDuration>"
      , "</StartApplicationSession>" ]


------------------------------------------------------------------------------
startAppSessionPosResp =
    B.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<StartApplicationSessionPosResponse xmlns=\"http://www.ecma-international.org/standards/ecma-354/appl_session\">"
      ,     "<sessionID>469421A9364D46D6444524AE41BEAD72-0</sessionID>"
      ,     "<actualProtocolVersion>", protocol, "</actualProtocolVersion>"
      ,     "<actualSessionDuration>180</actualSessionDuration>"
      , "</StartApplicationSessionPosResponse>" ]


startAppSessionNegResp =
    B.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<StartApplicationSessionNegResponse xmlns=\"http://www.ecma-international.org/standards/ecma-354/appl_session\">"
      ,     "<errorCode>"
      ,         "<definedError>invalidApplicationInfo</definedError>"
      ,     "</errorCode>"
      , "</StartApplicationSessionNegResponse>" ]


------------------------------------------------------------------------------
resetApplicationSessionTimer =
    B.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<ResetApplicationSessionTimer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
      ,   "xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns=\"http://www.ecma-international.org/standards/ecma-354/appl_session\">"
      ,     "<sessionID>469421A9364D46D6444524AE41BEAD72-0</sessionID>"
      ,     "<requestedSessionDuration>180</requestedSessionDuration>"
      , "</ResetApplicationSessionTimer>" ]


resetApplicationSessionTimerPosResponse = undefined
resetApplicationSessionTimerNegResponse = undefined


------------------------------------------------------------------------------
getDeviceIdMessage =
    B.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<GetDeviceId xmlns=\"http://www.avaya.com/csta\">"
      ,     "<switchName>", callServerIp ,"</switchName>"
      ,     "<extension>", extension, "</extension>"
      , "</GetDeviceId>" ]


getDeviceIdResponseMessage =
    B.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<GetDeviceIdResponse xmlns=\"http://www.avaya.com/csta\">"
      ,     "<device typeOfNumber=\"other\" mediaClass=\"voice\""
      ,       "bitRate=\"constant\">4750::111.2.33.444:0</device>"
      , "</GetDeviceIdResponse>" ]


------------------------------------------------------------------------------
monitorStartMessage deviceId =
    B.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<MonitorStart xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns=\"http://www.ecma-international.org/standards/ecma-323/csta/ed3\">"
      ,     "<monitorObject>"
      ,         "<deviceObject typeOfNumber=\"other\" mediaClass=\"notKnown\">", deviceId, "</deviceObject>"
      ,     "</monitorObject>"
      ,     "<requestedMonitorFilter>"
      ,         "<physicalDeviceFeature>"
      ,             "<displayUpdated>true</displayUpdated>"
      ,             "<hookswitch>false</hookswitch>"
      ,             "<lampMode>true</lampMode>"
      ,             "<ringerStatus>false</ringerStatus>"
      ,         "</physicalDeviceFeature>"
      ,     "</requestedMonitorFilter>"
      ,     "<extensions>"
      ,         "<privateData>"
      ,             "<private>"
      ,                 "<AvayaEvents>"
      ,                     "<invertFilter>true</invertFilter>"
      ,                 "</AvayaEvents>"
      ,             "</private>"
      ,         "</privateData>"
      ,     "</extensions>"
      , "</MonitorStart>" ]


monitorStartResponceMessage =
    B.concat
      [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      , "<MonitorStartResponse xmlns=\"http://www.ecma-international.org/standards/ecma-323/csta/ed3\">"
      ,     "<monitorCrossRefID>1</monitorCrossRefID>"
      ,     "<actualMonitorFilter>"
      ,         "<physicalDeviceFeature>"
      ,             "<displayUpdated>true</displayUpdated>"
      ,             "<hookswitch>false</hookswitch>"
      ,             "<lampMode>true</lampMode>"
      ,             "<ringerStatus>false</ringerStatus>"
      ,         "</physicalDeviceFeature>"
      ,     "</actualMonitorFilter>"
      ,     "<extensions>"
      ,         "<privateData>"
      ,             "<private>"
      ,                 "<AvayaEvents xmlns:ns1=\"http://www.avaya.com/csta\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"AvayaEvents\">"
      ,                     "<invertFilter>true</invertFilter>"
      ,                 "</AvayaEvents>"
      ,             "</private>"
      ,         "</privateData>"
      ,     "</extensions>"
      , "</MonitorStartResponse>" ]


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


------------------------------------------------------------------------------
toName s = Name {
             nameLocalName = s
           , nameNamespace = Just "http://www.ecma-international.org/standards/ecma-354/appl_session"
           , namePrefix = Nothing
           }
