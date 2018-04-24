{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-|

WebSockets interface for DMCC.

-}

module Main where

import           DMCC.Prelude hiding (getArgs)

import           Control.Monad.Logger.CallStack as CS

import           Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Configurator as Cfg
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Data.String (fromString)

import           Network.WebSockets

import           System.Environment
import           System.Exit
import           System.Posix.Signals
import           System.Random
import           Text.Printf

import           Paths_dmcc
import           DMCC


data Config = Config
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
    let logger = runStdoutLoggingT

    -- Terminate on SIGTERM
    let termHandler = Catch $ do
          logger $ CS.logInfo "Termination signal received"
          throwTo this ExitSuccess

    _ <- installHandler sigTERM termHandler Nothing
    logger $ realMain logger config

  _ -> getProgName >>= \pn -> error $ "Usage: " <> pn <> " <path to config>"


type AgentMap = TMVar (Map.Map AgentHandle Int)


-- | Read config and actually start the server
realMain :: (MonadLoggerIO m, MonadMask m, MonadBaseControl IO m)
         => (m () -> IO ())
         -> FilePath
         -> m ()
realMain logger config = do
  c <- liftIO $ Cfg.load [Cfg.Required config]
  cfg@Config{..} <- liftIO $ Config
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

  CS.logInfo $ "Running dmcc-" <> fromString (showVersion version)
  CS.logInfo $ "Starting session using " <> fromString (show cfg)

  let runSession = startSession
        (aesAddr, fromIntegral aesPort)
        (if aesTLS then TLS caDir else Plain)
        apiUser apiPass
        whUrl
        defaultSessionOptions { statePollingDelay       = stateDelay
                              , sessionDuration         = sessDur
                              , connectionRetryAttempts = connAtts
                              , connectionRetryDelay    = connDelay
                              }

      releaseSession s = do
        CS.logInfo $ "Stopping " <> fromString (show s)
        stopSession s

      handleSession s = do
        CS.logInfo $ "Running server for " <> fromString (show s)
        (agentMap :: AgentMap) <- liftIO $ newTMVarIO Map.empty
        liftIO $ runServer "0.0.0.0" listenPort $ logger . avayaApplication cfg s agentMap

  bracket runSession releaseSession handleSession


-- | Decrement reference counter for an agent. If no references left,
-- release control over agent. Return how many references are left.
releaseAgentRef :: (MonadLoggerIO m, MonadMask m) => AgentHandle -> AgentMap -> m Int
releaseAgentRef ah refs = do
  r <- atomically $ takeTMVar refs
  flip onException (atomically $ putTMVar refs r) $
    case Map.lookup ah r of
      Just cnt -> do
        newR <-
          if cnt > 1
             then pure $ Map.insert ah (cnt - 1) r
             else do releaseAgent ah
                     CS.logDebug $ "Agent " <> fromString (show ah) <> " is no longer controlled"
                     pure $ Map.delete ah r

        atomically $ putTMVar refs newR
        pure $ cnt - 1

      Nothing -> error $ "Releasing unknown agent " <> show ah


avayaApplication :: (MonadLoggerIO m, MonadMask m, MonadBaseControl IO m)
                 => Config
                 -> Session
                 -- ^ DMCC session.
                 -> AgentMap
                 -- ^ Reference-counting map of used agent ids.
                 -> PendingConnection
                 -> m ()
avayaApplication Config{..} as refs pending =
  case pathArg of
    [Nothing, Just ext] -> do
      -- A readable label for this connection for debugging purposes
      token <- liftIO $ randomRIO (1, 16 ^ (4 :: Int))

      let label = T.pack $ printf "%d/%04x" ext (token :: Int)
          -- Assume that all agents are on the same switch
          ext' = Extension $ T.pack $ show ext

      conn <- liftIO $ acceptRequest pending
      CS.logError $ "New websocket opened for " <> label

      -- Create a new agent reference, possibly establishing control over the agent
      r <- atomically $ takeTMVar refs

      let initialHandler = do
            CS.logDebug $ "Exception when plugging " <> label
            atomically $ putTMVar refs r
            CS.logDebug $ "Restored agent references map to " <> fromString (show r)

      (ah, evThread) <- flip onException initialHandler $ do
        cRsp <- controlAgent switchName ext' as
        ah <- case cRsp of
                Right ah' -> pure ah'
                Left err -> do
                  liftIO $ sendTextData conn $ encode $ RequestError $ show err

                  CS.logError $
                    "Could not control agent for " <> label <> ": " <> fromString (show err)

                  throwIO err

        -- Increment reference counter
        let oldCount = fromMaybe 0 $ Map.lookup ah r
        atomically $ putTMVar refs $ Map.insert ah (oldCount + 1) r
        CS.logDebug $ "Controlling agent " <> fromString (show ah) <> " from " <> label
        refReport ext' $ oldCount + 1

        -- Agent events loop
        evThread <-
          handleEvents ah $ \ev -> do
            CS.logInfo $ "Event for " <> label <> ": " <> fromString (show ev)
            liftIO $ sendTextData conn $ encode ev

        CS.logDebug $ fromString (show evThread) <> " handles events for " <> label
        pure (ah, evThread)

      let disconnectionHandler = do
            CS.logDebug $ "Websocket closed for " <> label
            killThread evThread
            threadDelay $ refDelay * 1000000
            -- Decrement reference counter when the connection dies or any
            -- other exception happens
            releaseAgentRef ah refs >>= refReport ext'

      handle (\(_ :: ConnectionException) -> disconnectionHandler) $ do
        s <- getAgentSnapshot ah
        liftIO $ sendTextData conn $ encode s

        -- Agent actions loop
        forever $ do
          msg <- liftIO $ receiveData conn

          case eitherDecode msg of
            Right act -> do
              CS.logDebug $ "Action from " <> label <> ": " <> fromString (show act)
              agentAction act ah

            Left e -> do
              CS.logDebug
                $ "Unrecognized message from " <> label <> ": "
                <> fromString (BL.unpack msg) <> " (" <> fromString e <> ")"

              liftIO $ sendTextData conn $ encode $ Map.fromList [("errorText" :: String, e)]

    _ -> liftIO $ rejectRequest pending "Malformed extension number"

  where
    pathArg = map (fmap fst . B.readInt) $ B.split '/' $ requestPath $ pendingRequest pending

    refReport ext cnt = CS.logDebug $
      fromString (show ext) <> " has " <> fromString (show cnt) <> " references"
