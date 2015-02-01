module DMCC.Agent

where

import           Control.Concurrent.STM (TChan)
import           Data.Text (Text)

import           DMCC.Types
import qualified DMCC.XML.Response as Rs
import {-# SOURCE #-} DMCC.Session


data Agent


type AgentHandle = (AgentId, Session)


monitorId :: Agent -> Text


inputChan :: Agent -> TChan Rs.Event


releaseAgent :: AgentHandle -> IO ()
