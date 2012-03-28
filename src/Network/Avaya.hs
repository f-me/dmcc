{-# LANGUAGE OverloadedStrings #-}

module Network.Avaya
    ( Avaya
    , AvayaException
    , Conf (..)
    , runClient
    , call
    ) where

import           Prelude hiding (catch, getContents)
import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe
import           Data.Word
import           Network
import           System.IO

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Error
import           Control.Monad.State
import           Data.ByteString.Lex.Integral (packDecimal, readDecimal_)
import qualified Data.Text as T
import           Text.XML
import           Text.XML.Cursor

import           Network.Avaya.Messages
import           Network.Avaya.Types

import Debug.Trace

debug = flip trace


call :: String -> Avaya ()
call ns = do
    setHookswitchStatus
    mapM_ (buttonPress . T.singleton) ns


------------------------------------------------------------------------------
-- | Four specific XML messages must be constructed by the application
-- and sent to the connector server in order to get started:
--
-- 1. StartApplicationSession;
-- 2. GetDeviceId;
-- 3. MonitorStart;
-- 4. RegisterTerminal - required only for device/media control
-- applications, and not for applications using call control
-- exclusively.
--
-- TODO: multiple devices in one session.
runClient :: Conf -> Avaya a -> IO (Either AvayaException ())
runClient conf act = withSocketsDo $ do
    h <- connectTo host (PortNumber port)
    hSetBuffering h LineBuffering

    input  <- newTChanIO
    output <- newTChanIO

    forkIO $ loopRead h input
    forkIO $ loopWrite h output 0

    startAvaya input output $ do
        startAppSession conf
        requestDeviceId conf
        monitorStart
        registerTerminal conf

        act

        cleanup
  where
    host = cHost conf
    port = cPort conf


loopRead h ch = do
    header <- B.hGet h 8  -- get header (first 8 bytes)

    when (B.length header /= 0) $ do
        let header' = runGet getHeader header

        res    <- B.hGet h (fromIntegral $ hLength header')

        if hInvokeId header' < 9999
          then atomically $ writeTChan ch res
          else putStrLn $ "* 9999:\n" ++ B.unpack res

        loopRead h ch


loopWrite h ch idx = do
    mess <- atomically $ readTChan ch
    B.hPut h (runPut $ putHeader mess (packInvokeId idx))
    loopWrite h ch (nextIdx idx)
  where
    nextIdx i = if i < 9998
                  then i + 1
                  else 0

    packInvokeId i = addNulls $ B.fromChunks [ fromJust $ packDecimal i ]

    addNulls x = B.replicate (4 - B.length x) '0' `B.append` x


-- Establish an application session.
startAppSession conf = do
    writeChanA $ startAppSessionMessage conf
    res <- readChanA

    setSessionId $ parseSessionId res
    setProtocol $ parseProtocol res

    liftIO $ putStrLn $ "* startAppSessionResponse:\n" ++ B.unpack res


requestDeviceId conf = do
    writeChanA $ getDeviceIdMessage conf
    res <- readChanA

    setDeviceId $ parseDeviceId res

    liftIO $ putStrLn $ "* getDeviceIdResponseMessage:\n" ++ B.unpack res


-- Request notification on events.
monitorStart = do
    mess <- monitorStartMessage <$> getProtocol <*> getDeviceId

    writeChanA mess
    res <- readChanA

    setMonitorId $ parseMonitorId res

    liftIO $ putStrLn $ "* monitorStartResponseMessage:\n" ++ B.unpack res


registerTerminal conf = do
    mess <- registerTerminalRequestMessage conf <$> getDeviceId

    liftIO $ putStrLn $ "* registerTerminalRequestMessage:\n" ++ B.unpack mess

    writeChanA mess
    res <- readChanA

    liftIO $ putStrLn $ "* registerTerminalResponseMessage:\n" ++ B.unpack res


setHookswitchStatus = do
    mess <- setHookswitchStatusMessage <$> getProtocol <*> getDeviceId

    writeChanA mess
    res <- readChanA

    liftIO $ putStrLn $ "* setHookswitchStatusResponseMessage:\n" ++ B.unpack res


buttonPress button = do
    mess <- buttonPressMessage button <$> getProtocol <*> getDeviceId

    writeChanA mess
    res <- readChanA

    liftIO $ putStrLn $ "* buttonPressResponseMessage:\n" ++ B.unpack res


cleanup = do
    -- Stop monitoring the events.
    monId <- getMonitorId
    writeChanA $ monitorStopMessage monId
    res <- readChanA
    liftIO $ putStrLn $ "* monitorStopResponseMessage:\n" ++ B.unpack res

    -- Release the device identifier.
    devId <- getDeviceId
    writeChanA $ releaseDeviceIdMessage devId
    res <- readChanA
    liftIO $ putStrLn $ "* releaseDeviceResponseMessage:\n" ++ B.unpack res

    -- Close the app session.
    sId <- getSessionId
    writeChanA $ stopAppSession sId
    res <- readChanA
    liftIO $ putStrLn $ "* stopAppSessionResponseMessage:\n" ++ B.unpack res


------------------------------------------------------------------------------
-- sendRequest :: B.ByteString -> Avaya ()
-- sendRequest request = do
--     h <- getHandle
--     invokeId <- nextInvokeId
--     liftIO $ B.hPut h (runPut $ putHeader request invokeId)


-- recvResponse :: Avaya B.ByteString
-- recvResponse = do
--     h <- getHandle
--     header <- liftIO $ runGet getHeader <$> B.hGet h 8  -- parse header (first 8 bytes)
--     liftIO $ B.hGet h (fromIntegral $ hLength header)


------------------------------------------------------------------------------
-- | All XML messages must have prepended the appropriate eight byte
-- header as specified by CSTA.
-- |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  | ...        |
-- |  Version  |   Length  |       InvokeID        | XML Message Body |
data CstaHeader = CstaHeader {
      hVersion  :: Word16
    , hLength   :: Word16
    , hInvokeId :: Int  -- ^ request and response XML messages must use Invoke IDs between 0 and 9998
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
    invokeId <- readDecimal_ <$> getByteString 4
    return $ CstaHeader version (length - 8) invokeId


------------------------------------------------------------------------------
parseResponse :: B.ByteString -> Document
parseResponse = parseLBS_ def  -- FIXME: maybe we should use parseLBS that returns Either SomeException Document?


isPosResponse res =
    bool $ fromDocument doc $| laxElement "StartApplicationSessionPosResponse"
  where
    doc = parseResponse res


parseSessionId  = sessionId . parseResponse
parseProtocol   = protocol . parseResponse
parseDeviceId   = deviceId  . parseResponse
parseMonitorId  = monitorId . parseResponse


sessionId doc = el doc "sessionID"
protocol  doc = el doc "actualProtocolVersion"
deviceId  doc = el doc "device"
monitorId doc = el doc "monitorCrossRefID"


el doc name =
    head $ fromDocument doc $/  laxElement name  -- FIXME: head on empty list
                            &// content
