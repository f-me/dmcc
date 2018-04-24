{-|

DMCC XML API implementation for third party call control and
monitoring.

-}

module DMCC
  ( Session
  , ConnectionType (..)
  , startSession
  , stopSession
  , defaultSessionOptions

  , AgentHandle
  , controlAgent
  , releaseAgent

  , Action (..)
  , agentAction

  , AgentEvent (..)
  , AgentSnapshot (..)
  , handleEvents
  , getAgentSnapshot

  , module DMCC.Types
  )

where

import           DMCC.Agent
import           DMCC.Session
import           DMCC.Types
