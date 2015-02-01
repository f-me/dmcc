{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|

WebSockets interface for DMCC.

TODO:

- Describe DMCC-WebSockets API

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
import           System.Random
import           Text.Printf

import           DMCC


data Config =
  Config
  { listenPort :: Int
  , aesAddr    :: String
  , aesPort    :: Int
  , aesTLS     :: Bool
  , caDir      :: Maybe FilePath
  , apiUser    :: Text
  , apiPass    :: Text
  , switchName :: SwitchName
  , logLibrary :: Bool
  }
  deriving Show


main :: IO ()
main = getArgs >>= \case
  [config] -> withSyslog "dmcc-ws" [PID] USER (logUpTo Debug) $ do
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
      <*> Cfg.lookupDefault True c "aes-use-tls"
      <*> Cfg.lookup  c "aes-cacert-directory"
      <*> Cfg.require c "api-user"
      <*> Cfg.require c "api-pass"
      <*> (SwitchName <$> Cfg.require c "switch-name")
      <*> Cfg.require c "log-library"

  bracket
    (syslog Info ("Starting session using " ++ show cfg) >>
     startSession aesAddr (fromIntegral aesPort)
     (if aesTLS then (TLS caDir) else Plain)
     apiUser apiPass
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
      -- Somewhat unique label for this connection
      token <- randomRIO (1, 16 ^ (4 :: Int))
      let label = printf "%d/%04x" ext (token :: Int)
      conn <- acceptRequest pending
      syslog Debug $ "New websocket opened for " ++ label
      forkPingThread conn 30
      -- Assume that all agents are on the same switch
      ah <- controlAgent (switchName cfg) (Extension ext) as
      syslog Debug $ "Controlling agent " ++ show ah ++ " from " ++ label
      s <- getAgentState ah
      sendTextData conn $ encode s
      -- Event/action loops
      evThread <- handleEvents ah
        (\ev ->
           do
             syslog Debug ("Event for " ++ label ++ ": " ++ show ev)
             sendTextData conn $ encode ev)
      handle
        (\e ->
           do
             killThread evThread
             syslog Debug ("Exception for " ++ label ++ ": " ++
                           show (e :: ConnectionException))) $
        forever $ do
          msg <- receiveData conn
          case decode msg of
            Just act -> do
              syslog Debug $ "Action from " ++ label ++ ": " ++ show act
              agentAction act ah
            _ -> syslog Debug $
                 "Unrecognized message from " ++ label ++ ": " ++
                 BL.unpack msg
    _ -> rejectRequest pending "Malformed extension number"
