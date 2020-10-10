module DMCC.Util where

import           DMCC.Prelude

import           Data.Text as T
import           Control.Monad.Logger.CallStack as CS
import           DMCC.Types

maybeSyslog :: MonadLogger m => Maybe LoggingOptions -> String -> m ()
maybeSyslog Nothing _ = pure ()
maybeSyslog (Just _) msg = CS.logInfo $ T.pack msg
