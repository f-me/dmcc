module DMCC.Util

where

import           System.Posix.Syslog

import           DMCC.Types

maybeSyslog :: Maybe LoggingOptions -> Priority -> String -> IO ()
maybeSyslog Nothing _ _ = return ()
maybeSyslog (Just LoggingOptions{..}) pri msg =
  withSyslog syslogIdent [PID] USER (logUpTo Debug) $ syslog pri msg
