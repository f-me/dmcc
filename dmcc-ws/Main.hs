{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-|

WebSockets interface for DMCC.

-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Concurrent.STM

import           Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Configurator as Cfg
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

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
  , whUrl      :: Maybe String
  , refDelay   :: Int
  , stateDelay :: Int
  , switchName :: SwitchName
  , logLibrary :: Bool
  , sessDur    :: Int
  , connAtts   :: Int
  , connDelay  :: Int
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
      <*> Cfg.lookup  c "web-hook-handler-url"
      <*> Cfg.lookupDefault 1 c "agent-release-delay"
      <*> Cfg.lookupDefault 1 c "state-polling-delay"
      <*> (SwitchName <$> Cfg.require c "switch-name")
      <*> Cfg.require c "log-library"
      <*> Cfg.require c "session-duration"
      <*> Cfg.require c "connection-retry-attempts"
      <*> Cfg.require c "connection-retry-delay"

  bracket
    (syslog Info ("Starting session using " ++ show cfg) >>
     startSession (aesAddr, fromIntegral aesPort)
     (if aesTLS then (TLS caDir) else Plain)
     apiUser apiPass
     whUrl
     (if logLibrary then Just defaultLoggingOptions else Nothing)
     defaultSessionOptions{ statePollingDelay = stateDelay
                          , sessionDuration = sessDur
                          , connectionRetryAttempts = connAtts
                          , connectionRetryDelay = connDelay
                          })
    (\s ->
       syslog Info ("Stopping " ++ show s) >>
       stopSession s)
    (\s ->
       syslog Info ("Running server for " ++ show s) >>
       newTMVarIO (Map.empty) >>=
       \refs -> runServer "0.0.0.0" listenPort (avayaApplication cfg s refs))


type AgentMap = TMVar (Map.Map AgentHandle Int)


-- | Decrement reference counter for an agent. If no references left,
-- release control over agent. Return how many references are left.
releaseAgentRef :: AgentHandle -> AgentMap -> IO Int
releaseAgentRef ah refs = do
  r <- atomically $ takeTMVar refs
  flip onException (atomically $ putTMVar refs r) $
    case Map.lookup ah r of
      Just cnt -> do
        newR <-
          if cnt > 1
          then return $ Map.insert ah (cnt - 1) r
          else (releaseAgent ah >>
                (syslog Debug $
                 "Agent " ++ show ah ++ " is no longer controlled") >>
                (return $ Map.delete ah r))
        atomically $ putTMVar refs newR
        return $ cnt - 1
      Nothing -> error $ "Releasing unknown agent " ++ show ah


avayaApplication :: Config
                 -> Session
                 -- ^ DMCC session.
                 -> AgentMap
                 -- ^ Reference-counting map of used agent ids.
                 -> ServerApp
avayaApplication cfg as refs pending = do
  let rq = pendingRequest pending
      pathArg = map (liftM fst . B.readInt) $ B.split '/' $ requestPath rq
      refReport ext cnt =
        syslog Debug $ show ext ++ " has " ++ show cnt ++ " references"
  case pathArg of
    [Nothing, Just ext] -> do
      -- A readable label for this connection for debugging purposes
      token <- randomRIO (1, 16 ^ (4 :: Int))
      let label = printf "%d/%04x" ext (token :: Int)
      conn <- acceptRequest pending
      syslog Debug $ "New websocket opened for " ++ label
      forkPingThread conn 30
      -- Create agent reference
      r <- atomically $ takeTMVar refs
      flip onException (atomically $ putTMVar refs r) $ do
        -- Assume that all agents are on the same switch
        let ext' = Extension $ T.pack $ show ext
        cRsp <- controlAgent (switchName cfg) ext' as
        ah <- case cRsp of
                Right ah' -> return ah'
                Left err -> do
                  sendTextData conn $ encode $ RequestError $ show err
                  syslog Error $ "Could not control agent for " ++
                    label ++ ": " ++ show err
                  throwIO err
        -- Increment reference counter
        let oldCount = fromMaybe 0 $ Map.lookup ah r
        atomically $ putTMVar refs (Map.insert ah (oldCount + 1) r)
        syslog Debug $ "Controlling agent " ++ show ah ++ " from " ++ label
        refReport ext' (oldCount + 1)
        -- Agent events loop
        evThread <- handleEvents ah
          (\ev ->
             do
               syslog Debug ("Event for " ++ label ++ ": " ++ show ev)
               sendTextData conn $ encode ev)
        let disconnectionHandler = do
              syslog Debug $ "Websocket closed for " ++ label
              killThread evThread
              threadDelay $ refDelay cfg * 1000000
              -- Decrement reference counter when the connection dies or any
              -- other exception happens
              releaseAgentRef ah refs >>= refReport ext'
        handle (\(_ :: ConnectionException) -> disconnectionHandler) $ do
          s <- getAgentSnapshot ah
          sendTextData conn $ encode s
          -- Agent actions loop
          forever $ do
            msg <- receiveData conn
            case eitherDecode msg of
              Right act -> do
                syslog Debug $ "Action from " ++ label ++ ": " ++ show act
                agentAction act ah
              Left e -> do
                syslog Debug $
                  "Unrecognized message from " ++ label ++ ": " ++
                  BL.unpack msg ++ " (" ++ e ++ ")"
                sendTextData conn $ encode $
                  Map.fromList [("errorText" :: String, e)]
    _ -> rejectRequest pending "Malformed extension number"
