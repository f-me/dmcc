{-# LANGUAGE ConstraintKinds #-}

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

type MonadCatchLoggerIO m = (MonadCatch m, MonadLoggerIO m)
