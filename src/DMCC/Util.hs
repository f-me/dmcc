module DMCC.Util where

import           Data.Text as T
import           Control.Monad.Logger.CallStack
import           DMCC.Prelude
import           DMCC.Types

maybeSyslog :: MonadLogger m => Maybe LoggingOptions -> String -> m ()
maybeSyslog Nothing _ = return ()
maybeSyslog (Just LoggingOptions{..}) msg = logInfo (T.pack msg)
