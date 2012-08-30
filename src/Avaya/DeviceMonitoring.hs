
module Avaya.DeviceMonitoring
  (startDeviceMonitoring
  ,stopDeviceMonitoring
  ) where

import Control.Monad
import Control.Concurrent
import Data.Text (Text) 

import Avaya.MessageLoop
import qualified Avaya.Messages.Request as Rq
import qualified Avaya.Messages.Response as Rs

data MonitoringHandle = MonitoringHandle
  {actualProtocolVersion :: Text
  ,sessionId :: Text
  ,deviceId :: Text
  ,monitorId :: Text
  ,pingThread :: ThreadId
  }


startDeviceMonitoring
  :: LoopHandle
  -> Text -> Text -> Text -> Text -> Text
  -> IO (Either Int MonitoringHandle)
startDeviceMonitoring h user pass switch ext pwd = do
  Rs.StartApplicationSessionPosResponse{..} <- sendRequestSync h
    $ Rq.StartApplicationSession
      {applicationId = "Test"
      ,requestedProtocolVersion = Rq.V4_2
      ,userName = user
      ,password = pass
      ,sessionCleanupDelay = 180
      ,requestedSessionDuration = 180
      }

  Rs.GetDeviceIdResponse{..} <- sendRequestSync h
    $ Rq.GetDeviceId
      {switchName = switch
      ,extension = ext
      }
  Rs.MonitorStartResponse{..} <- sendRequestSync h
    $ Rq.MonitorStart
      {acceptedProtocol = actualProtocolVersion
      ,deviceObject = device
      }
  Rs.RegisterTerminalResponse{..} <- sendRequestSync h
    $ Rq.RegisterTerminalRequest
      {device = device
      ,password = pwd
      }

  case code of
    1 -> do
      pingThread <- forkIO $ forever $ do
        threadDelay $ actualSessionDuration * 300 * 1000
        sendRequestAsync h
          $ Rq.ResetApplicationSessionTimer
            {sessionId = sessionID
            ,requestedSessionDuration = actualSessionDuration
            }
      return $ Right $ MonitoringHandle
        actualProtocolVersion
        sessionID
        device
        monitorCrossRefID
        pingThread

    _ -> do
      -- FIXME: session cleanup
      return $ Left code 

stopDeviceMonitoring :: LoopHandle -> MonitoringHandle -> IO ()
stopDeviceMonitoring h (MonitoringHandle{..}) = do
  killThread pingThread

  sendRequestSync h $ Rq.UnregisterTerminalRequest
    {device = deviceId
    }
  sendRequestSync h $ Rq.MonitorStop
    {acceptedProtocol = actualProtocolVersion
    ,monitorCrossRefID = monitorId
    }
  sendRequestSync h $ Rq.ReleaseDeviceId
    {device = deviceId
    }
  sendRequestSync h $ Rq.StopApplicationSession
    {sessionID = sessionId
    }
  return ()
