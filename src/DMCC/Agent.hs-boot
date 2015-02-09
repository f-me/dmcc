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


rspChan :: Agent -> TChan Rs.Response


releaseAgent :: AgentHandle -> IO ()
