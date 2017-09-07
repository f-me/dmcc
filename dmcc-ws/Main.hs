{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-|

WebSockets interface for DMCC.

-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Logger.CallStack as CS
import           Control.Concurrent.STM

import           Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Configurator as Cfg
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)

import           Network.WebSockets

import           System.Environment
import           System.Exit
import           System.Posix.Signals
import           System.Random
import           Text.Printf

import           Paths_dmcc
import           DMCC
import           DMCC.Prelude (liftIO)


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
  [config] -> do
    this <- myThreadId
    -- Terminate on SIGTERM
    _ <- installHandler
         sigTERM
         (Catch (runStdoutLoggingT (CS.logInfo (T.pack "Termination signal received")) >>
                 throwTo this ExitSuccess))
          Nothing
    realMain config
  _ -> getProgName >>= \pn -> error $ "Usage: " ++ pn ++ " <path to config>"


-- | Read config and actually start the server
realMain :: FilePath -> IO ()
realMain config = do
  c <- Cfg.load [Cfg.Required config]
  cfg@Config{..} <- Config
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
    (runStdoutLoggingT $ CS.logInfo (T.pack $ "Running dmcc-" ++ showVersion version) >>
     CS.logInfo (T.pack $ "Starting session using " ++ show cfg) >>
     startSession (aesAddr, fromIntegral aesPort)
     (if aesTLS then TLS caDir else Plain)
     apiUser apiPass
     whUrl
     defaultSessionOptions { statePollingDelay = stateDelay
                          , sessionDuration = sessDur
                          , connectionRetryAttempts = connAtts
                          , connectionRetryDelay = connDelay
                          })
    (\s ->
        runStdoutLoggingT $ CS.logInfo (T.pack $ "Stopping " ++ show s) >>
        stopSession s)
    (\s ->
       (runStdoutLoggingT . CS.logInfo . T.pack) ("Running server for " ++ show s) >>
       newTMVarIO Map.empty >>=
       \refs -> runServer "0.0.0.0" listenPort (avayaApplication cfg s refs))


type AgentMap = TMVar (Map.Map AgentHandle Int)


-- | Decrement reference counter for an agent. If no references left,
-- release control over agent. Return how many references are left.
releaseAgentRef :: MonadLoggerIO m => AgentHandle -> AgentMap -> m Int
releaseAgentRef ah refs = do
  r <- liftIO . atomically $ takeTMVar refs
  liftIO $ flip onException (atomically $ putTMVar refs r) $
    case Map.lookup ah r of
      Just cnt -> do
        newR <-
          if cnt > 1
          then return $ Map.insert ah (cnt - 1) r
          else runStdoutLoggingT $ releaseAgent ah >>
               runStdoutLoggingT (CS.logDebug (T.pack $ "Agent " ++ show ah ++ " is no longer controlled")) >>
               return (Map.delete ah r)
        liftIO . atomically $ putTMVar refs newR
        return $ cnt - 1
      Nothing -> error $ "Releasing unknown agent " ++ show ah


avayaApplication :: Config
                 -> Session
                 -- ^ DMCC session.
                 -> AgentMap
                 -- ^ Reference-counting map of used agent ids.
                 -> ServerApp
avayaApplication Config{..} as refs pending = do
  let rq = pendingRequest pending
      pathArg = map (fmap fst . B.readInt) $ B.split '/' $ requestPath rq
      refReport ext cnt =
        CS.logDebug (T.pack $ show ext ++ " has " ++ show cnt ++ " references")
  case pathArg of
    [Nothing, Just ext] -> do
      -- A readable label for this connection for debugging purposes
      token <- randomRIO (1, 16 ^ (4 :: Int))
      let label = printf "%d/%04x" ext (token :: Int)
          -- Assume that all agents are on the same switch
          ext' = Extension $ T.pack $ show ext
      conn <- acceptRequest pending
      runStdoutLoggingT $ CS.logError (T.pack $ "New websocket opened for " ++ label)
      -- Create a new agent reference, possibly establishing control
      -- over the agent
      r <- atomically $ takeTMVar refs
      let initialHandler = do
            runStdoutLoggingT $ CS.logDebug (T.pack $ "Exception when plugging " ++ label)
            atomically $ putTMVar refs r
            runStdoutLoggingT $ CS.logDebug (T.pack $ "Restored agent references map to " ++ show r)
      (ah, evThread) <- (`onException` initialHandler) $ do
        cRsp <- runStdoutLoggingT $ controlAgent switchName ext' as
        ah <- case cRsp of
                Right ah' -> return ah'
                Left err -> do
                  sendTextData conn $ encode $ RequestError $ show err
                  runStdoutLoggingT $ CS.logError (T.pack $ "Could not control agent for " ++ label ++ ": " ++ show err)
                  throwIO err
        -- Increment reference counter
        let oldCount = fromMaybe 0 $ Map.lookup ah r
        atomically $ putTMVar refs (Map.insert ah (oldCount + 1) r)
        runStdoutLoggingT $ CS.logDebug (T.pack $ "Controlling agent " ++ show ah ++ " from " ++ label)
        runStdoutLoggingT $ refReport ext' (oldCount + 1)
        -- Agent events loop
        evThread <- handleEvents ah
          (\ev ->
             do
               runStdoutLoggingT $ CS.logInfo (T.pack $ "Event for " ++ label ++ ": " ++ show ev)
               sendTextData conn $ encode ev)
        runStdoutLoggingT $ CS.logDebug (T.pack $ show evThread ++ " handles events for " ++ label)
        return (ah, evThread)

      let disconnectionHandler = do
            runStdoutLoggingT $ CS.logDebug (T.pack $ "Websocket closed for " ++ label)
            killThread evThread
            threadDelay $ refDelay * 1000000
            -- Decrement reference counter when the connection dies or any
            -- other exception happens
            runStdoutLoggingT $ releaseAgentRef ah refs >>= runStdoutLoggingT . refReport ext'
      handle (\(_ :: ConnectionException) -> disconnectionHandler) $ do
        s <- runStdoutLoggingT $ getAgentSnapshot ah
        sendTextData conn $ encode s
        -- Agent actions loop
        forever $ do
          msg <- receiveData conn
          case eitherDecode msg of
            Right act -> do
              runStdoutLoggingT $ CS.logDebug (T.pack $ "Action from " ++ label ++ ": " ++ show act)
              runStdoutLoggingT $ agentAction act ah
            Left e -> do
              runStdoutLoggingT $ CS.logDebug (T.pack $ "Unrecognized message from " ++ label ++ ": " ++
                BL.unpack msg ++ " (" ++ e ++ ")")
              sendTextData conn $ encode $
                Map.fromList [("errorText" :: String, e)]
    _ -> rejectRequest pending "Malformed extension number"
