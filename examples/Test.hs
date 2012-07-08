{-# LANGUAGE OverloadedStrings #-}

module Test where

import Network.Avaya as A
import Network.Avaya.Action as A

test = do
  Right st <- A.startAvaya conf
  A.runAvayaAction st (A.call "989161111111" >> cleanup)

conf = AvayaConfig
  { cHost         = "192.168.20.5"
  , cPort         = 4721
  , cUser         = "avaya"
  , cUserPassword = "avayapassword"
  , cDelay        = "5"
  , cVersion      = "4.2"
  , cDuration     = "180"
  , cCallServerIp = "S8300ADAC"
  , cExtension    = "143"
  , cPassword     = "1234567"
  }
