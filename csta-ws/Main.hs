{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|

WebSockets interface for CSTA.

TODO:

- Describe CSTA-WebSockets API

- Handle signals

- Logging

- Unique client IDs in log messages

- Provide access to agent state (calls list)

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
import           System.Posix.Syslog

import           CSTA


data Config =
  Config
  { listenPort :: Int
  , aesAddr    :: String
  , aesPort    :: Int
  , aesUser    :: Text
  , aesPass    :: Text
  , aesSwitch  :: SwitchName
  }
  deriving Show


main :: IO ()
main = getArgs >>= \case
  [config] -> realMain config
  _ -> getProgName >>= \pn -> error $ "Usage: " ++ pn ++ " <path to config>"


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

  withSyslog "csta-ws" [PID] USER (logUpTo Debug) $
    bracket
      (syslog Info ("Starting session using " ++ show cfg) >>
       startSession aesAddr aesPort aesUser aesPass)
      (\as ->
         do
           syslog Info $ "Stopping " ++ show as
           stopSession as)
      (\as ->
         do
           syslog Info $ "Running server for " ++ show as
           runServer "0.0.0.0" listenPort $ avayaApplication cfg as)


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
