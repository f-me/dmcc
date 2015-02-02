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

  , AgentHandle
  , controlAgent
  , releaseAgent

  , Action(..)
  , agentAction

  , Event(..)
  , handleEvents
  , getAgentState

  , module DMCC.Types
  )

where

import           DMCC.Agent
import           DMCC.Session
import           DMCC.Types
