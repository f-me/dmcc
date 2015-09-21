{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

{-|

DMCC XML API implementation for third party call control and
monitoring.

-}

module DMCC
  ( Session
  , ConnectionType(..)
  , startSession
  , stopSession
  , defaultLoggingOptions
  , defaultSessionOptions

  , AgentHandle
  , controlAgent
  , releaseAgent

  , Action(..)
  , TimestampedAction(..)
  , agentAction

  , AgentEvent(..)
  , AgentSnapshot(..)
  , handleEvents
  , getAgentSnapshot

  , module DMCC.Types
  )

where

import           DMCC.Agent
import           DMCC.Session
import           DMCC.Types
