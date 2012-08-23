
module Avaya.MessageLoop
  (LoopHandle
  ,LoopEvent(..)
  ,DeviceState(..)
  ,AvayaError(..)
  ,startMessageLoop
  ,shutdownLoop
  ,attachObserver
  ,sendRequestSync
  )where


import Control.Arrow
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.IntMap as Map

import System.IO
import Network

import Avaya.Messages
import qualified Avaya.Messages.Raw as Raw


data LoopHandle = LoopHandle
  {socket :: Handle
  ,readThread :: ThreadId
  ,procThread :: ThreadId
  ,observers  :: TVar [Observer]
  ,waitingRequests  :: TVar (Map.IntMap (TMVar Response))
  ,deviceState :: TVar DeviceState
  ,invokeId :: TVar Int
  }

data DeviceState = DeviceState
  deriving Show

defaultDeviceState = DeviceState

  
type Observer = LoopEvent -> IO ()

data LoopEvent
  = AvayaRsp Response
--  | AvayaReq Request
  | Timeout
  | ReadError
  | ShutdownRequested
  deriving Show

data AvayaError = AvayaError

--FIXME: handle network errors
startMessageLoop :: String -> PortNumber -> IO (Either AvayaError LoopHandle)
startMessageLoop host port = withSocketsDo $ do
  sock <- connectTo host (PortNumber $ port)
  hSetBuffering sock NoBuffering

  msgChan <- newTChanIO
  readThread <- forkIO $ forever
      $ Raw.readResponse sock
      >>= atomically . writeTChan msgChan . first AvayaRsp

  observers <- newTVarIO []
  waitingRequests <- newTVarIO Map.empty
  deviceState <- newTVarIO defaultDeviceState
  invokeId <- newTVarIO 0

  procThread <- forkIO $ forever $ do
      (msg,invokeId) <- atomically $ readTChan msgChan
      atomically $ modifyTVar' deviceState (updateDevice msg)
      obs <- readTVarIO observers
      mapM_ (forkIO . ($msg)) obs
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
shutdownLoop h = do
  -- syncronyously push ShutdownRequested to all observers
  readTVarIO (observers h) >>= mapM_ ($ShutdownRequested)
  killThread $ readThread h
  killThread $ procThread h
  hClose $ socket h


attachObserver :: LoopHandle -> Observer -> IO ()
attachObserver h o = atomically $ modifyTVar' (observers h) (o:)


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

