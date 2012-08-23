
module Avaya.MessageLoop
  (LoopHandle
  ,LoopEvent(..)
  ,DeviceState(..)
  ,AvayaError(..)
  ,startMessageLoop
  ,shutdownLoop
  ,attachObserver
  ,sendRequest
  )where


import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy  as L

import System.IO
import Network

import Avaya.Messages
import qualified Avaya.Messages.Raw as Raw


data LoopHandle = LoopHandle
  {socket :: Handle
  ,msgChan :: TChan LoopEvent
  ,readThread :: ThreadId
  ,procThread :: ThreadId
  ,observers  :: TVar [Observer]
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

data AvayaError = AvayaError

--FIXME: handle network errors
startMessageLoop :: String -> PortNumber -> IO (Either AvayaError LoopHandle)
startMessageLoop host port = withSocketsDo $ do
  sock <- connectTo host (PortNumber $ port)
  hSetBuffering sock NoBuffering

  msgChan <- newTChanIO
  readThread <- forkIO $ forever
      $ Raw.readResponse sock
      >>= atomically . writeTChan msgChan . AvayaRsp

  observers <- newTVarIO []
  deviceState <- newTVarIO defaultDeviceState
  invokeId <- newTVarIO 0

  procThread <- forkIO $ forever $ do
      msg <- atomically $ readTChan msgChan
      atomically $ modifyTVar' deviceState (updateDevice msg)
      obs <- atomically $ readTVar observers
      mapM_ (forkIO . ($msg)) obs

  let h = LoopHandle
        sock msgChan
        readThread procThread
        observers deviceState invokeId

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


sendRequest :: LoopHandle -> Request -> IO ()
sendRequest h rq = do
  ix <- atomically $ do
    modifyTVar' (invokeId h) ((`mod` 9999).(+1))
    readTVar (invokeId h)
  -- FIXME: handle error
  Raw.sendRequest (socket h) ix rq

