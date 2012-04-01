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
import           Network
import           System.IO

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Error
import           Control.Monad.State
import           Data.ByteString.Lex.Integral (packDecimal, readDecimal_)
import qualified Data.Text as T

import           Network.Avaya.Messages
import           Network.Avaya.Parse
import           Network.Avaya.Types

import Debug.Trace

debug = flip trace


call :: String    -- ^ telephone number
     -> Avaya ()
call ns = do
    setHookswitchStatus False
    mapM_ (buttonPress . T.singleton) ns


------------------------------------------------------------------------------
-- | Four specific XML messages must be constructed by the application
-- and sent to the connector server in order to get started:
--
-- 1. StartApplicationSession;
--
-- 2. GetDeviceId;
--
-- 3. MonitorStart;
--
-- 4. RegisterTerminal - required only for device/media control
-- applications, and not for applications using call control
-- exclusively.
--
-- TODO: multiple devices in one session.
runClient :: Conf -> (Event -> IO ()) -> Avaya a -> IO (Either AvayaException ())
runClient conf callback act = withSocketsDo $ do
    h <- connectTo host (PortNumber port)
    hSetBuffering h LineBuffering

    input  <- newTChanIO
    output <- newTChanIO

    forkIO $ loopRead h input callback
    forkIO $ loopWrite h output 0

    startAvaya input output $ do
        startAppSession conf
        requestDeviceId conf
        monitorStart
        registerTerminal conf

        act    -- TODO: loop action

        liftIO $ getLine

        cleanup
  where
    host = cHost conf
    port = cPort conf


loopRead :: Handle -> TChan B.ByteString -> (Event -> IO ()) -> IO ()
loopRead h ch callback = do
    res <- B.hGet h 8  -- get header (first 8 bytes)

    unless (B.null res) $ do
        let header = runGet getHeader res

        mes <- B.hGet h $ hLength header

        if hInvokeId header < 9999
          then atomically $ writeTChan ch mes
          else do
            -- putStrLn $ "* 9999:\n" ++ B.unpack mes
            callback $ parseEvent mes

        loopRead h ch callback


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


setHookswitchStatus :: Bool -> Avaya ()
setHookswitchStatus isOnHook = do
    mess <- setHookswitchStatusMessage onHook <$> getProtocol <*> getDeviceId

    writeChanA mess
    res <- readChanA

--    changeStatus HookswitchUp

    liftIO $ putStrLn $ "* setHookswitchStatusResponseMessage:\n" ++ B.unpack res
  where
    onHook = if isOnHook then "true" else "false"

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
