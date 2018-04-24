{-|

Types and instances for implementing webhook interface for DMCC.

-}

module DMCC.WebHook
  ( WHEvent (..)
  , AgentEvent (..)
  , Event (..)
  )

where

import           DMCC.Agent
import           DMCC.XML.Response
