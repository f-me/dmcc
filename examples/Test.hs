{-# LANGUAGE OverloadedStrings #-}

import Network.Avaya

test = runClient conf $ call "989150011111"
  where
    conf = Conf { cHost         = "192.168.20.5"
                , cPort         = 4721
                , cUser         = "username"
                , cUserPassword = "password"
                , cDelay        = "5"
                , cVersion      = "4.2"
                , cDuration     = "180"
                , cCallServerIp = "192.168.20.2"
                , cExtension    = "142"
                , cPassword     = "1234567"
                }
