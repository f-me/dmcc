

import Control.Monad
import Control.Concurrent

import Avaya.MessageLoop
import Avaya.DeviceMonitoring

main :: IO ()
main = do
  Right h <- startMessageLoop "127.0.0.1" 4721
  attachObserver h print
  startDeviceMonitoring h
    "user" "pass" "S8300ADAC" "125" "1234567"
  forever $ threadDelay 100
