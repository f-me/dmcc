
module Avaya.DeviceMonitoring where

import Control.Monad
import Control.Concurrent
import Data.Text (Text) 

import Avaya.MessageLoop
import qualified Avaya.Messages.Request as Rq
import qualified Avaya.Messages.Response as Rs

startDeviceMonitoring
  :: LoopHandle
  -> Text -> Text -> Text -> Text -> Text
  -> IO (Either Int (Text,Text))
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
  sendRequestSync h
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
      forkIO $ forever $ do
        threadDelay $ actualSessionDuration * 300 * 1000
        sendRequestAsync h
          $ Rq.ResetApplicationSessionTimer
            {sessionId = sessionID
            ,requestedSessionDuration = actualSessionDuration
            }
      return $ Right (actualProtocolVersion,device)
    _ -> do
      -- FIXME: session cleanup
      return $ Left code 
