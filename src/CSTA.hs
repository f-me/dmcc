{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

{-|

CSTA XML API implementation for third party call control and
monitoring.

TODO:

- Logging

-}

module CSTA
  ( Session
  , startSession
  , stopSession
  , defaultLoggingOptions

  , controlAgent
  , releaseAgent

  , Action(..)
  , agentAction

  , Event(..)
  , handleEvents
  , getAgentState

  , module CSTA.Types
  )

where

import           CSTA.Agent
import           CSTA.Session
import           CSTA.Types
