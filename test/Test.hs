

import Control.Monad
import Control.Concurrent

import qualified Data.Text as T
import System.Environment

import Avaya.MessageLoop
import Avaya.DeviceMonitoring

main :: IO ()
main = do
  [ext] <- getArgs
  Right h <- startMessageLoop "127.0.0.1" 4721
  attachObserver h print
  startDeviceMonitoring h
    "user" "pass" "S8300ADAC" (T.pack ext) "1234567"
  forever $ threadDelay 1000000
