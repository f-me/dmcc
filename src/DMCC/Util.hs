module DMCC.Util

where

import           System.Posix.Syslog
import           Foreign.C.String as FS
import           DMCC.Types

maybeSyslog :: Maybe LoggingOptions -> Priority -> String -> IO ()
maybeSyslog Nothing _ _ = return ()
maybeSyslog (Just LoggingOptions{..}) pri msg =
  withSyslog syslogIdent [LogPID] User $ ((withCStringLen msg) $ syslog (Just User) pri)
