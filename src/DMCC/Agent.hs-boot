{-# LANGUAGE FlexibleContexts #-}

module DMCC.Agent

where

import           DMCC.Prelude

import           DMCC.Types
import qualified DMCC.XML.Response as Rs
import {-# SOURCE #-} DMCC.Session


data Agent


newtype AgentHandle = AgentHandle (AgentId, Session)


monitorId :: Agent -> Text


rspChan :: Agent -> TChan Rs.Response


releaseAgent :: (MonadUnliftIO m, MonadLoggerIO m, MonadBaseControl IO m, MonadCatch m)
             => AgentHandle -> m ()
