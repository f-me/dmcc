{-# LANGUAGE OverloadedStrings #-}

import Network.Avaya

test = runClient conf print $ call "989150011111"
  where
    conf = Conf { cHost         = "192.168.20.5"
                , cPort         = 4721
                , cUser         = "username"
                , cUserPassword = "password"
                , cDelay        = "5"
                , cVersion      = "4.2"
                , cDuration     = "180"
                , cCallServerIp = "S8300ADAC"
                , cExtension    = "142"
                , cPassword     = "1234567"
                }
