
module Avaya.DeviceMonitoring where

import Control.Monad
import Control.Concurrent

import Avaya.MessageLoop
import qualified Avaya.Messages.Request as Rq
import qualified Avaya.Messages.Response as Rs

-- TODO: monitoring handler
--       reset timeout thread
--       stop monitoring
-- startDeviceMonitoring :: LoopHandle -> Text -> Text -> IO ()
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

  forkIO $ forever $ do
    threadDelay $ actualSessionDuration * 300
    sendRequestAsync h
      $ Rq.ResetApplicationSessionTimer
        {sessionId = sessionID
        ,requestedSessionDuration = actualSessionDuration
        }

  Rs.GetDeviceIdResponse{..} <- sendRequestSync h
    $ Rq.GetDeviceId
      {switchName = switch
      ,extension = ext
      }
  sendRequestSync h
    $ Rq.MonitorStart
      {acceptedProtocol = actualProtocolVersion
      ,deviceObject = device
      }
  sendRequestSync h
    $ Rq.RegisterTerminalRequest
      {device = device
      ,password = pwd
      }
