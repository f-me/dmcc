
module Avaya.MessageLoop
  (LoopHandle
  ,LoopEvent(..)
  ,DeviceState(..)
  ,AvayaError(..)
  ,startMessageLoop
  ,shutdownLoop
  ,ObserverId
  ,attachObserver
  ,detachObserver
  ,sendRequestSync
  ,sendRequestAsync
  )where


import Control.Arrow
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.IntMap.Strict as Map

import System.IO
import Network

import Avaya.Messages.Request (Request)
import Avaya.Messages.Response (Response)
import qualified Avaya.Messages.Raw as Raw


data LoopHandle = LoopHandle
  {socket :: Handle
  ,readThread :: ThreadId
  ,procThread :: ThreadId
  ,observers  :: TVar (Map.IntMap Observer)
  ,waitingRequests  :: TVar (Map.IntMap (TMVar Response))
  ,deviceState :: TVar DeviceState
  ,invokeId :: TVar Int
  }

data DeviceState = DeviceState
  deriving Show


defaultDeviceState :: DeviceState
defaultDeviceState =  DeviceState


type Observer = LoopEvent -> IO ()
newtype ObserverId = ObserverId Int

data LoopEvent
  = AvayaRsp Response
--  | AvayaReq Request
  | Timeout
  | ReadError
  | ShutdownRequested
  deriving Show

data AvayaError = AvayaError

--FIXME: handle network errors
startMessageLoop :: String -> Int -> IO (Either AvayaError LoopHandle)
startMessageLoop host port = withSocketsDo $ do
  sock <- connectTo host (PortNumber $ fromIntegral port)
  hSetBuffering sock NoBuffering

  msgChan <- newTChanIO
  readThread <- forkIO $ forever
      $ Raw.readResponse sock
      >>= atomically . writeTChan msgChan . first AvayaRsp

  observers <- newTVarIO Map.empty
  waitingRequests <- newTVarIO Map.empty
  deviceState <- newTVarIO defaultDeviceState
  invokeId <- newTVarIO 0

  procThread <- forkIO $ forever $ do
      (msg,invokeId) <- atomically $ readTChan msgChan
      atomically $ modifyTVar' deviceState (updateDevice msg)
      obs <- readTVarIO observers
      mapM_ (forkIO . ($msg)) $ Map.elems obs
      case msg of
        AvayaRsp rsp -> do
          syncs <- readTVarIO waitingRequests
          case Map.lookup invokeId syncs of
            Nothing -> return ()
            Just sync -> void $ atomically $ tryPutTMVar sync rsp
        _ -> return ()

  let h = LoopHandle
        sock
        readThread procThread
        observers waitingRequests
        deviceState invokeId

  return $ Right h


updateDevice :: LoopEvent -> DeviceState -> DeviceState
updateDevice e = id

shutdownLoop :: LoopHandle -> IO ()
shutdownLoop (LoopHandle{..}) = do
  -- syncronyously push ShutdownRequested to all observers
  readTVarIO observers >>= mapM_ ($ShutdownRequested) . Map.elems
  killThread procThread
  killThread readThread
  hClose socket


attachObserver :: LoopHandle -> Observer -> IO ObserverId
attachObserver h o = atomically $ do
  m <- readTVar $ observers h
  let k = maybe 0 ((+1).fst.fst) $ Map.maxViewWithKey m
  writeTVar (observers h) $! Map.insert k o m
  return $ ObserverId k


detachObserver :: LoopHandle -> ObserverId -> IO ()
detachObserver h (ObserverId k)
  = atomically
  $ modifyTVar' (observers h) $ Map.delete k

sendRequestSync :: LoopHandle -> Request -> IO Response
sendRequestSync h rq = do
  (ix,var) <- atomically $ do
    modifyTVar' (invokeId h) ((`mod` 9999).(+1))
    ix <- readTVar (invokeId h)
    var <- newEmptyTMVar
    modifyTVar' (waitingRequests h) (Map.insert ix var)
    return (ix,var)
  -- FIXME: handle error
  Raw.sendRequest (socket h) ix rq
  atomically $ takeTMVar var

sendRequestAsync :: LoopHandle -> Request -> IO ()
sendRequestAsync h rq = do
  ix <- atomically $ do
    modifyTVar' (invokeId h) ((`mod` 9999).(+1))
    readTVar (invokeId h)
  -- FIXME: handle error
  Raw.sendRequest (socket h) ix rq


