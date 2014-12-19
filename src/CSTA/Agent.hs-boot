module CSTA.Agent

where

import           Control.Concurrent.STM (TChan)
import           Data.Text (Text)

import           CSTA.Types
import qualified CSTA.XML.Response as Rs
import {-# SOURCE #-} CSTA.Session


data Agent


type AgentHandle = (AgentId, Session)


monitorId :: Agent -> Text


inputChan :: Agent -> TChan Rs.Event


releaseAgent :: AgentHandle -> IO ()
