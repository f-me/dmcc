

import Avaya.MessageLoop
import Avaya.Messages.Request

main = do
  Right h <- startMessageLoop "127.0.0.1" $ fromIntegral (1234 :: Int)
  attachObserver h print
  rsp <- sendRequestSync h $ 
    StartApplicationSession
    {applicationId = "Test"
    ,requestedProtocolVersion = V5_2
    ,userName = "user"
    ,password = "pass"
    ,sessionCleanupDelay = 180
    ,requestedSessionDuration = 180
    }
  print rsp

{-
  connect
  attach observer
  start session
 -}
