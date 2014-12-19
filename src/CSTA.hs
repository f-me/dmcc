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

  , controlAgent
  , releaseAgent

  , Action(..)
  , agentAction

  , Rs.Event
  , handleEvents
  , getAgentCalls

  , module CSTA.Types
  )

where

import           CSTA.Agent
import           CSTA.Session
import           CSTA.Types
import qualified CSTA.XML.Response as Rs
