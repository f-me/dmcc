{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DMCC.Prelude
  ( module ClassyPrelude
  , module Control.Concurrent.STM.Lifted
  , MonadCatchLoggerIO
  )

where

import ClassyPrelude hiding ( atomically
                            , newBroadcastTChanIO
                            , newEmptyTMVarIO
                            , newTBQueueIO
                            , newTChanIO
                            , newTMVarIO
                            , newTQueueIO
                            , newTVarIO
                            , readTVarIO
                            )
import Control.Concurrent.STM.Lifted
import Control.Monad.Logger

instance MonadLogger IO where
  monadLoggerLog _ _ _ = return $ return ()

instance MonadLoggerIO IO where
  askLoggerIO = return monadLoggerLog

type MonadCatchLoggerIO m = (MonadCatch m, MonadLoggerIO m)
