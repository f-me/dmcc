{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|

WebSockets interface for CSTA.

TODO:

- Describe CSTA-WebSockets API

- Report agent state after handshake

-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Configurator as Cfg
import           Data.Text (Text)

import           Network.WebSockets
import           System.Environment
import           System.Exit
import           System.Posix.Syslog
import           System.Posix.Signals

import           CSTA


data Config =
  Config
  { listenPort :: Int
  , aesAddr    :: String
  , aesPort    :: Int
  , aesUser    :: Text
  , aesPass    :: Text
  , aesSwitch  :: SwitchName
  , logLibrary :: Bool
  }
  deriving Show


main :: IO ()
main = getArgs >>= \case
  [config] -> withSyslog "csta-ws" [PID] USER (logUpTo Debug) $ do
    this <- myThreadId
    -- Terminate on SIGTERM
    _ <- installHandler
         sigTERM
         (Catch (syslog Notice "Termination signal received" >>
                 throwTo this ExitSuccess))
         Nothing
    realMain config
  _ -> getProgName >>= \pn -> error $ "Usage: " ++ pn ++ " <path to config>"


-- | Read config and actually start the server
realMain :: FilePath -> IO ()
realMain config = do
  c <- Cfg.load [Cfg.Required config]
  cfg@(Config{..}) <- Config
      <$> Cfg.require c "listen-port"
      <*> Cfg.require c "aes-addr"
      <*> Cfg.require c "aes-port"
      <*> Cfg.require c "aes-user"
      <*> Cfg.require c "aes-pass"
      <*> (SwitchName <$> Cfg.require c "aes-switch")
      <*> Cfg.require c "log-library"

  bracket
    (syslog Info ("Starting session using " ++ show cfg) >>
     startSession aesAddr aesPort aesUser aesPass
     (if logLibrary then Just defaultLoggingOptions else Nothing))
    (\s ->
       syslog Info ("Stopping " ++ show s) >>
       stopSession s)
    (\s ->
       syslog Info ("Running server for " ++ show s) >>
       runServer "0.0.0.0" listenPort (avayaApplication cfg s))


avayaApplication :: Config -> Session -> ServerApp
avayaApplication cfg as pending = do
  let rq = pendingRequest pending
      pathArg = map (liftM fst . B.readInt) $ B.split '/' $ requestPath rq
  case pathArg of
    [Nothing, Just ext] -> do
      conn <- acceptRequest pending
      syslog Debug $ "New websocket opened for " ++ show ext
      forkPingThread conn 30
      -- Assume that all agents are on the same switch
      ah <- controlAgent (aesSwitch cfg) (Extension ext) as
      syslog Debug $ "Controlling agent " ++ show ah
      s <- getAgentState ah
      sendTextData conn $ encode s
      -- Event/action loops
      evThread <- handleEvents ah $
        (\ev ->
           do
             syslog Debug ("Event for " ++ show ext ++ ": " ++ show ev)
             sendTextData conn $ encode ev)
      handle
        (\e ->
           do
             killThread evThread
             syslog Debug ("Exception for " ++ show ext ++ ": " ++
                           show (e :: ConnectionException))) $
        forever $ do
          msg <- receiveData conn
          case decode msg of
            Just act -> do
              syslog Debug $ "Action from " ++ show ext ++ ": " ++ show act
              agentAction act ah
            _ -> syslog Debug $
                 "Unrecognized message from " ++ show ext ++ ": " ++
                 BL.unpack msg
    _ -> rejectRequest pending "Malformed extension number"
