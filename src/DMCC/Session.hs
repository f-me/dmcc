{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections #-}

{-|

DMCC session handling.

-}

module DMCC.Session
  ( Session(..)
  , ConnectionType(..)
  , startSession
  , stopSession
  , defaultLoggingOptions
  , defaultSessionOptions

  , DMCCError(..)

  , DMCCHandle(..)
  , sendRequestSync
  , sendRequestAsync
  )

where

import           DMCC.Prelude

import           Control.Arrow hiding (first)
import           Control.Monad.Logger

import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import           Data.Text as T (Text, empty)
import           Data.Typeable

import           System.IO
import           System.IO.Streams (InputStream,
                                    OutputStream,
                                    ReadTooShortException,
                                    write)
import           System.IO.Streams.Handle
import qualified System.IO.Streams.SSL as SSLStreams

import           Network
import qualified Network.HTTP.Client as HTTP
import           Network.Socket hiding (connect)
import           OpenSSL
import qualified OpenSSL.Session as SSL

import           DMCC.Types
import           DMCC.XML.Request (Request)
import qualified DMCC.XML.Request as Rq
import           DMCC.XML.Response (Response)
import qualified DMCC.XML.Response as Rs
import qualified DMCC.XML.Raw as Raw

import {-# SOURCE #-} DMCC.Agent


data ConnectionType = Plain
                    | TLS { caDir :: Maybe FilePath }


type ConnectionData = (InputStream ByteString, OutputStream ByteString, IO ())


-- | Low-level DMCC API plumbing.
data DMCCHandle = DMCCHandle
  { connection :: TMVar ConnectionData
  -- ^ AVAYA server socket streams and connection cleanup action.
  , dmccSession :: TMVar (Text, Int)
  -- ^ DMCC session ID and duration.
  , reconnect :: forall m. MonadCatchLoggerIO m => m ()
  -- ^ Reconnect to AVAYA server, changing socket streams, cleanup
  -- action and session.
  , pingThread :: ThreadId
  , readThread :: ThreadId
  -- ^ DMCC response reader thread.
  , procThread :: ThreadId
  -- ^ Response handler/synchronous requests worker thread.
  , invokeId :: TVar Int
  -- ^ Request/response counter.
  , syncResponses :: TVar (IntMap.IntMap (TMVar (Maybe Response)))
  , agentRequests :: TVar (IntMap.IntMap AgentId)
  -- ^ Keeps track of which request has has been issued by what agent
  -- (if there's one) until its response arrives.
  , sessionOptions :: SessionOptions
  }


-- | Library API session.
data Session = Session
  { protocolVersion :: Text
  , dmccHandle :: DMCCHandle
  , webHook :: Maybe (HTTP.Request, HTTP.Manager)
  -- ^ Web hook handler URL and manager.
  , agents :: TVar (Map.Map AgentId Agent)
  , agentLocks :: TVar (Set.Set AgentId)
  }


instance Show Session where
  show as =
    "Session{protocolVersion=" ++ unpack (protocolVersion as) ++ "}"


data LoopEvent
  = DMCCRsp Response
  | Timeout
  | ReadError
  deriving Show


data DMCCError = ApplicationSessionFailed
                 deriving (Show, Typeable)


instance Exception DMCCError


defaultSessionOptions :: SessionOptions
defaultSessionOptions = SessionOptions 1 120 24 5


startSession :: MonadLoggerIO m
             => (String, PortNumber)
             -- ^ Host and port of AES server.
             -> ConnectionType
             -- ^ Use TLS.
             -> Text
             -- ^ DMCC API user.
             -> Text
             -- ^ DMCC API password.
             -> Maybe String
             -- ^ Web hook URL.
             -> SessionOptions
             -> m Session
startSession (host, port) ct user pass whUrl sopts = do
  syncResponses <- newTVarIO IntMap.empty
  agentRequests <- newTVarIO IntMap.empty
  invoke <- newTVarIO 0

  -- When this is empty, I/O streams to the server are not ready.
  conn <- newEmptyTMVarIO
  -- When this is empty, DMCC session is not ready or is being
  -- recovered.
  sess <- newEmptyTMVarIO

  let
    -- Connect to the server, produce I/O streams and a cleanup action
    connect :: MonadCatchLoggerIO m => m ConnectionData
    connect = connect1 (connectionRetryAttempts sopts)
      where
        connectExHandler :: (Exception e, Show e, MonadCatchLoggerIO m) =>
                            Int -> e -> m ConnectionData
        connectExHandler attempts e = do
          logErrorN ("Connection failed: " <> tshow e)
          if attempts > 0
          then do
            threadDelay $ connectionRetryDelay sopts * 1000000
            connect1 (attempts - 1)
          else throwIO e
        connect1 attempts =
          handleNetwork (connectExHandler attempts) $
          liftIO $ case ct of
            Plain -> do
              h <- connectTo host (PortNumber $ fromIntegral port)
              hSetBuffering h NoBuffering
              is <- handleToInputStream h
              os <- handleToOutputStream h
              let cl = hClose h
              return (is, os, cl)
            TLS caDir -> withOpenSSL $ do
              sslCtx <- SSL.context
              SSL.contextSetDefaultCiphers sslCtx
              SSL.contextSetVerificationMode sslCtx $
                SSL.VerifyPeer True True Nothing
              maybe (return ()) (SSL.contextSetCADirectory sslCtx) caDir
              (is, os, ssl) <- SSLStreams.connect sslCtx host port
              let cl = do
                    SSL.shutdown ssl SSL.Unidirectional
                    maybe (return ()) close $ SSL.sslSocket ssl
              return (is, os, cl)

    -- Start new DMCC session
    startDMCCSession :: Maybe Text
                     -- ^ Previous session ID (we attempt to recover
                     -- when this is given).
                     -> IO ((Text, Int), Text)
    startDMCCSession old = do
      let
        sendReq =
          sendRequestSyncRaw
          conn
          reconnect
          invoke
          syncResponses
          Nothing
        startReq sid =
          sendReq
          Rq.StartApplicationSession
          { applicationId = ""
          , requestedProtocolVersion = Rq.DMCC_6_2
          , userName = user
          , password = pass
          , sessionCleanupDelay = sessionDuration sopts
          , sessionID = fromMaybe T.empty sid
          , requestedSessionDuration = sessionDuration sopts
          }
        -- Start a session monitor to enable TransferMonitorObjects
        -- feature
        sessionMonitorReq proto =
          sendReq
          Rq.MonitorStart
          { acceptedProtocol = proto
          , monitorRq = Rq.Session
          }
      startRsp <- startReq old
      case (startRsp, old) of
        (Just Rs.StartApplicationSessionPosResponse{..}, _) -> do
          sessionMonitorReq actualProtocolVersion
          return ((sessionID, actualSessionDuration), actualProtocolVersion)
        (Just Rs.StartApplicationSessionNegResponse, Just oldID) -> do
          -- The old session has expired, start from scratch
          startRsp' <- startReq Nothing
          case startRsp' of
            Just Rs.StartApplicationSessionPosResponse{..} -> do
              sessionMonitorReq actualProtocolVersion
              -- Transfer MonitorObjects from old session
              sendRequestAsyncRaw
                conn
                reconnect
                invoke
                Nothing
                Rq.TransferMonitorObjects
                { fromSessionID = oldID
                , toSessionID = sessionID
                , acceptedProtocol = actualProtocolVersion
                }
              return ((sessionID, actualSessionDuration), actualProtocolVersion)
            _ -> throwIO ApplicationSessionFailed
        _ -> throwIO ApplicationSessionFailed

    -- Restart I/O and DMCC session. This routine returns when new I/O
    -- streams become available (starting DMCC session requires
    -- response reader thread to be functional).
    reconnect = do
      logWarnN "Attempting reconnection"
      -- Only one reconnection at a time
      (oldId, cl) <- atomically $ do
        (oldId, _) <- takeTMVar sess
        (_, _, cl) <- takeTMVar conn
        return (oldId, cl)
      -- Fail all pending synchronous requests
      atomically $ do
        srs <- readTVar syncResponses
        mapM_ (`putTMVar` Nothing) $ IntMap.elems srs
        writeTVar syncResponses IntMap.empty
      handle
        (\(e :: IOException) ->
           logErrorN $
           "Failed to close old connection: " <> tshow e)
        cl
      -- We do not change the protocol version during session recovery
      connect >>= atomically . putTMVar conn
      logWarnN "Connection re-established"
      let
        shdl (Right ()) = return ()
        shdl (Left e) = throwIO e
      -- Fork new thread for DMCC session initialization. This is
      -- because 'reconnect' needs to return for response reader
      -- thread to start working again (which is required for DMCC
      -- session start).
      void $ flip forkFinally shdl $ do
        (newSession, _) <- startDMCCSession (Just oldId)
        atomically $ putTMVar sess newSession

  -- Read DMCC responses from socket
  msgChan <- newTChanIO
  let readExHandler e =
        logErrorN ("Read error: " <> tshow e) >>
        reconnect
  readThread <-
    fork $ forever $ liftIO $ do
      (istream, _, _) <- atomically $ readTMVar conn
      handleNetwork readExHandler $
        Raw.readResponse istream >>=
        atomically . writeTChan msgChan . first DMCCRsp

  agents <- newTVarIO Map.empty

  -- Process parsed messages
  procThread <- fork $ forever $ do
    (msg, invokeId) <- atomically $ readTChan msgChan
    -- TODO Check agent locks?
    ags <- readTVarIO agents
    case msg of
      DMCCRsp rsp -> do
        -- Return response if the request was synchronous
        sync' <- atomically $ do
          srs <- readTVar syncResponses
          modifyTVar' syncResponses (IntMap.delete invokeId)
          return $ IntMap.lookup invokeId srs
        case sync' of
          Just sync -> void $ atomically $ tryPutTMVar sync $ Just rsp
          Nothing -> return ()
        -- Redirect events and request errors to matching agent
        ag' <- case rsp of
                 Rs.EventResponse monId _ ->
                   return $ find (\a -> monId == monitorId a) $ Map.elems ags
                 Rs.CSTAErrorCodeResponse _ -> do
                   aid <- atomically $ do
                     ars <- readTVar agentRequests
                     modifyTVar' agentRequests (IntMap.delete invokeId)
                     return $ IntMap.lookup invokeId ars
                   return $ (`Map.lookup` ags) =<< aid
                 _ -> return Nothing
        case ag' of
          Just ag -> atomically $ writeTChan (rspChan ag) rsp
          -- Error/event received for an unknown agent?
          Nothing -> return ()
      _ -> return ()

  -- Keep the session alive
  pingThread <- fork $ forever $ do
    -- Do not send a keep-alive message if the session is not ready
    (sid, duration) <- readTMVarIO sess
    sendRequestAsyncRaw conn reconnect invoke Nothing
      Rq.ResetApplicationSessionTimer
      { sessionId = sid
      , requestedSessionDuration = duration
      }
    threadDelay $ duration * 500 * 1000

  let h = DMCCHandle
          conn
          sess
          reconnect
          pingThread
          readThread
          procThread
          invoke
          syncResponses
          agentRequests
          sopts

  -- Start the session
  (liftIO connect) >>= atomically . putTMVar conn
  (newSession, actualProtocolVersion) <- startDMCCSession Nothing
  atomically $ putTMVar sess newSession

  wh <- case whUrl of
          Just url -> do
            mgr <- HTTP.newManager HTTP.defaultManagerSettings
            req <- HTTP.parseUrl url
            return $ Just (req, mgr)
          Nothing -> return Nothing

  agLocks <- newTVarIO Set.empty

  return $
    Session
    actualProtocolVersion
    h
    wh
    agents
    agLocks


-- | TODO Agent releasing notice
stopSession :: (MonadBase IO m, MonadCatchLoggerIO m) => Session -> m ()
stopSession as@Session{..} = do
  -- Release all agents
  ags <- readTVarIO agents
  (s, _) <- atomically $ readTMVar $ dmccSession dmccHandle
  mapM_ (liftIO . releaseAgent . (\aid -> AgentHandle (aid, as))) $ keys ags

  sendRequestAsync dmccHandle Nothing Rq.StopApplicationSession{sessionID = s}

  -- TOOD Use async/throwTo instead
  killThread $ pingThread dmccHandle
  killThread $ procThread dmccHandle
  killThread $ readThread dmccHandle
  (_, ostream, cleanup) <- atomically $ readTMVar (connection dmccHandle)
  liftIO $ do
    write Nothing ostream
    cleanup


-- | Send a request and block until the response arrives or a write
-- exception occurs. No request is sent until a connection and an
-- application session become available. Write exceptions cause a
-- reconnection and a session restart, Nothing is returned in this
-- case.
--
-- This must not be used to for session setup as this requires an
-- active DMCC application session!
--
-- Write errors are made explicit here because 'sendRequestSync' is
-- called from multiple locations, making it tedious to install the
-- reconnection handler everywhere.
sendRequestSync :: MonadCatchLoggerIO m
                => DMCCHandle
                -> Maybe AgentId
                -- ^ Push erroneous responses to this agent's event
                -- processor.
                --
                -- TODO Disallow unknown agents on type level (use
                -- AgentHandle).
                -> Request
                -> m (Maybe Response)
sendRequestSync DMCCHandle{..} aid rq = do
  void $ atomically $ readTMVar dmccSession
  sendRequestSyncRaw
    connection
    reconnect
    invokeId
    syncResponses
    ((agentRequests, ) <$> aid)
    rq


sendRequestSyncRaw :: MonadCatchLoggerIO m
                   => TMVar ConnectionData
                   -- ^ Block until this connection becomes available.
                   -> m ()
                   -- ^ Reconnection action.
                   -> TVar Int
                   -> TVar (IntMap.IntMap (TMVar (Maybe Response)))
                   -- ^ Synchronous response storage.
                   -> Maybe (TVar (IntMap.IntMap AgentId), AgentId)
                   -- ^ Agent requests map and target agent id. When
                   -- provided, CSTAErrorCode response will be
                   -- directed to that agent's event processor.
                   -> Request
                   -> m (Maybe Response)
sendRequestSyncRaw connection re invoke srs ar !rq = do
  (ix, var, c@(_, ostream, _)) <- atomically $ do
    modifyTVar' invoke ((`mod` 9999) . (+1))
    ix <- readTVar invoke
    var <- newEmptyTMVar
    modifyTVar' srs (IntMap.insert ix var)
    case ar of
      Just (ars, a) -> modifyTVar' ars (IntMap.insert ix a)
      Nothing -> return ()
    c <- takeTMVar connection
    return (ix, var, c)
  let
    srHandler e = do
      logErrorN $ "Write error: " <> tshow e
      liftIO $ atomically $ do
        putTMVar connection c
        putTMVar var Nothing
        modifyTVar' srs $ IntMap.delete ix
        case ar of
          Just (ars, _) -> modifyTVar' ars (IntMap.delete ix)
          Nothing -> return ()
      re
  handleNetwork srHandler $ Raw.sendRequest ostream ix rq
  -- Release the connection at once and wait for response in a
  -- separate transaction.
  liftIO $ do
    atomically $ putTMVar connection c
    atomically $ takeTMVar var


-- | Like 'sendRequestAsync', but do not wait for a result.
sendRequestAsync :: MonadCatchLoggerIO m
                 => DMCCHandle
                 -> Maybe AgentId
                 -- ^ Push erroneous responses to this agent's event
                 -- processor.
                 -> Request
                 -> m ()
sendRequestAsync DMCCHandle{..} aid rq = do
  void $ readTMVarIO dmccSession
  sendRequestAsyncRaw
    connection
    reconnect
    invokeId
    ((agentRequests, ) <$> aid)
    rq


sendRequestAsyncRaw :: MonadCatchLoggerIO m
                    => TMVar ConnectionData
                    -- ^ Block until this connection becomes available.
                    -> m ()
                    -- ^ Reconnection action.
                    -> TVar Int
                    -> Maybe (TVar (IntMap.IntMap AgentId), AgentId)
                    -> Request
                    -> m ()
sendRequestAsyncRaw connection re invoke ar !rq = do
  (ix, c@(_, ostream, _)) <- atomically $ do
    modifyTVar' invoke ((`mod` 9999) . (+1))
    ix <- readTVar invoke
    case ar of
      Just (ars, a) -> modifyTVar' ars (IntMap.insert ix a)
      Nothing -> return ()
    c <- takeTMVar connection
    return (ix, c)
  let
    srHandler e = do
      logErrorN $ "Write error: " <> tshow e
      liftIO $ atomically $ do
        putTMVar connection c
        case ar of
          Just (ars, _) -> modifyTVar' ars (IntMap.delete ix)
          Nothing -> return ()
      re
  handleNetwork srHandler $ Raw.sendRequest ostream ix rq
  atomically $ putTMVar connection c


-- | Handle network-related errors we know of.
handleNetwork :: forall a m. MonadCatchLoggerIO m
              => (forall e. (Exception e, Show e) => (e -> m a))
              -- ^ Exception handler.
              -> m a
              -> m a
handleNetwork handler action =
  action `catches`
  [ Handler (\(e :: ReadTooShortException) -> handler e)
  , Handler (\(e :: IOException) -> handler e)
  , Handler (\(e :: SSL.ConnectionAbruptlyTerminated) -> handler e)
  , Handler (\(e :: SSL.ProtocolError) -> handler e)
  ]
