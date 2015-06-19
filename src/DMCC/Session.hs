{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  , DMCCHandle(..)
  , sendRequestSync
  , sendRequestAsync
  )

where

import           Control.Arrow
import           Control.Monad
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Functor

import           Data.ByteString (ByteString)
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import           Data.Text (Text, unpack)

import           System.IO
import           System.IO.Streams (InputStream, OutputStream, write)
import           System.IO.Streams.Handle
import qualified System.IO.Streams.SSL as SSLStreams
import           System.Posix.Syslog

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


-- | Low-level DMCC API plumbing.
data DMCCHandle = DMCCHandle
  { connection :: TMVar (InputStream ByteString, OutputStream ByteString, IO ())
  -- ^ AVAYA server socket streams and connection cleanup action.
  , dmccSession :: TMVar (Text, Int)
  -- ^ DMCC session ID and duration.
  , reconnect :: IO ()
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
  , loggingOptions :: Maybe LoggingOptions
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
    "Session{protocolVersion=" ++ (unpack $ protocolVersion as) ++ "}"


data LoopEvent
  = DMCCRsp Response
  | Timeout
  | ReadError
  deriving Show


defaultLoggingOptions :: LoggingOptions
defaultLoggingOptions = LoggingOptions "dmcc-lib"


defaultSessionOptions :: SessionOptions
defaultSessionOptions = SessionOptions 1 12 10


startSession :: (String, PortNumber)
             -- ^ Host and port of AES server.
             -> ConnectionType
             -- ^ Use TLS.
             -> Text
             -- ^ DMCC API user.
             -> Text
             -- ^ DMCC API password.
             -> Maybe String
             -- ^ Web hook URL.
             -> Maybe LoggingOptions
             -> SessionOptions
             -> IO Session
startSession (host, port) ct user pass whUrl lopts sopts = withOpenSSL $ do
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
    connect :: IO (InputStream ByteString, OutputStream ByteString, IO ())
    connect = connect1 (connectionRetryAttempts sopts)
      where
        connect1 attempts =
          handle
          -- Attempt to connect several times
          (\(e :: SomeException) -> do
               Raw.maybeSyslog lopts Critical ("Connection failed: " ++ show e)
               if attempts > 0
               then do
                 threadDelay $ (connectionRetryDelay sopts) * 1000000
                 connect1 (attempts - 1)
               else throwIO e) $
          case ct of
            Plain -> do
              h <- connectTo host (PortNumber $ fromIntegral port)
              hSetBuffering h NoBuffering
              is <- handleToInputStream h
              os <- handleToOutputStream h
              let cl = hClose h
              return (is, os, cl)
            TLS caDir -> do
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
    startDMCCSession :: OutputStream ByteString
                     -> Maybe Text
                     -- ^ Previous session ID (we attempt to recover
                     -- when this is given).
                     -> IO ((Text, Int), Text)
    startDMCCSession ostream old = do
      Just Rs.StartApplicationSessionPosResponse{..} <-
        sendRequestSyncRaw
        lopts
        ostream
        reconnect
        invoke
        syncResponses
        Nothing $
        Rq.StartApplicationSession
        { applicationId = ""
        , requestedProtocolVersion = Rq.DMCC_6_2
        , userName = user
        , password = pass
        , sessionCleanupDelay = 80
        , oldSessionID = old
        , requestedSessionDuration = 80
        }

      return ((sessionID, actualSessionDuration), actualProtocolVersion)

    -- Restart I/O and DMCC session. This routine returns when new I/O
    -- streams become available (starting DMCC session requires
    -- response reader thread to be functional).
    reconnect = do
      Raw.maybeSyslog lopts Warning "Attempting reconnection"
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
        (\(e :: SomeException) ->
           Raw.maybeSyslog lopts Error $
           "Failed to close old connection: " ++ show e)
        cl
      -- We do not change the protocol version during session recovery
      c@(_, ostream, _) <- connect
      atomically $ putTMVar conn c
      Raw.maybeSyslog lopts Warning "Connection re-established"
      void $ forkIO $ do
        (newSession, _) <- startDMCCSession ostream (Just oldId)
        atomically $ putTMVar sess newSession

  -- Read DMCC responses from socket
  msgChan <- newTChanIO
  let exHandler e =
        Raw.maybeSyslog lopts Critical ("Read error: " ++ show e) >>
        reconnect
  readThread <-
    forkIO $ forever $ do
      (istream, _, _) <- atomically $ readTMVar conn
      handle (\(e :: SomeException) -> exHandler e) $
        Raw.readResponse lopts istream >>=
        atomically . writeTChan msgChan . first DMCCRsp

  agents <- newTVarIO Map.empty

  -- Process parsed messages
  procThread <- forkIO $ forever $ do
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
  pingThread <- forkIO $ forever $ do
    -- Do not send a keep-alive message if I/O is unavailable or the
    -- session is not ready yet.
    (ostream, (sid, duration)) <- atomically $ do
      (_, ostream, _) <- readTMVar conn
      s <- readTMVar sess
      return (ostream, s)
    sendRequestAsyncRaw lopts ostream reconnect invoke Nothing $
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
          lopts
          sopts

  -- Start the session
  c@(_, ostream, _) <- connect
  atomically $ putTMVar conn c
  (newSession, actualProtocolVersion) <- startDMCCSession ostream Nothing
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
stopSession :: Session -> IO ()
stopSession as@(Session{..}) = do
  -- Release all agents
  ags <- readTVarIO agents
  (s, _) <- atomically $ readTMVar $ dmccSession dmccHandle
  mapM_ releaseAgent $
    zipWith (\aid as -> AgentHandle (aid, as)) (Map.keys ags) (repeat as)

  sendRequestAsync dmccHandle Nothing $
    Rq.StopApplicationSession{sessionID = s}
  killThread $ pingThread dmccHandle
  killThread $ procThread dmccHandle
  killThread $ readThread dmccHandle
  (_, ostream, cleanup) <- atomically $ readTMVar (connection dmccHandle)
  write Nothing ostream
  cleanup


-- | Send a request and block until the response arrives or a write
-- exception occurs. Write exceptions cause a reconnection and a
-- session restart, Nothing is returned in this case.
--
-- Write errors are made explicit here because 'sendRequestSync' is
-- called from multiple locations.
sendRequestSync :: DMCCHandle
                -> Maybe AgentId
                -- ^ Push erroneous responses to this agent's event
                -- processor.
                --
                -- TODO Disallow unknown agents on type level (use
                -- AgentHandle).
                -> Request
                -> IO (Maybe Response)
sendRequestSync (DMCCHandle{..}) aid rq = do
  (_, ostream, _) <- atomically $ readTMVar dmccSession >> readTMVar connection
  sendRequestSyncRaw
    loggingOptions
    ostream
    reconnect
    invokeId
    syncResponses
    ((agentRequests, ) <$> aid)
    rq


sendRequestSyncRaw :: Maybe LoggingOptions
                   -> OutputStream ByteString
                   -> IO ()
                   -- ^ Reconnection action.
                   -> TVar Int
                   -> TVar (IntMap.IntMap (TMVar (Maybe Response)))
                   -> Maybe (TVar (IntMap.IntMap AgentId), AgentId)
                   -> Request
                   -> IO (Maybe Response)
sendRequestSyncRaw lopts ostream re invoke srs ar rq = do
  (ix, var) <- atomically $ do
    modifyTVar' invoke ((`mod` 9999) . (+1))
    ix <- readTVar invoke
    var <- newEmptyTMVar
    modifyTVar' srs (IntMap.insert ix var)
    case ar of
      Just (ars, a) -> modifyTVar' ars (IntMap.insert ix a)
      Nothing -> return ()
    return (ix, var)
  handle (\(e :: SomeException) -> do
              Raw.maybeSyslog lopts Critical ("Write error: " ++ show e)
              atomically $ do
                putTMVar var Nothing
                modifyTVar' srs $ IntMap.delete ix
                case ar of
                  Just (ars, _) -> modifyTVar' ars (IntMap.delete ix)
                  Nothing -> return ()
              re) $
    Raw.sendRequest lopts ostream ix rq
  atomically $ takeTMVar var


sendRequestAsync :: DMCCHandle
                 -> Maybe AgentId
                 -- ^ Push erroneous responses to this agent's event
                 -- processor.
                 -> Request
                 -> IO ()
sendRequestAsync (DMCCHandle{..}) aid rq = do
  (_, ostream, _) <- atomically $ readTMVar dmccSession >> readTMVar connection
  sendRequestAsyncRaw
    loggingOptions
    ostream
    reconnect
    invokeId
    ((agentRequests, ) <$> aid)
    rq


sendRequestAsyncRaw :: Maybe LoggingOptions
                    -> OutputStream ByteString
                    -> IO ()
                    -> TVar Int
                    -> Maybe (TVar (IntMap.IntMap AgentId), AgentId)
                    -> Request
                    -> IO ()
sendRequestAsyncRaw lopts ostream re invoke ar rq = do
  ix <- atomically $ do
    modifyTVar' invoke ((`mod` 9999) . (+1))
    ix' <- readTVar invoke
    case ar of
      Just (ars, a) -> modifyTVar' ars (IntMap.insert ix' a)
      Nothing -> return ()
    return ix'
  handle (\(e :: SomeException) -> do
              Raw.maybeSyslog lopts Critical ("Write error: " ++ show e)
              atomically $ case ar of
                             Just (ars, _) -> modifyTVar' ars (IntMap.delete ix)
                             Nothing -> return ()
              re) $
    Raw.sendRequest lopts ostream ix rq
