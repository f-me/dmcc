{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Shared type declarations. -}

module DMCC.Types

where

import Data.Aeson
import Data.Aeson.TH
import Data.CaseInsensitive
import Data.Data
import Data.Functor
import Data.Text
import Data.Time.Clock


-- | Device ID used in DMCC requests.
--
-- This is based on text as stated in DMCC specification.
newtype DeviceId =
  DeviceId (CI Text)
  deriving (Eq, Ord, Show)


instance FromJSON DeviceId where
  parseJSON v = (DeviceId . mk) <$> parseJSON v


instance ToJSON DeviceId where
  toJSON (DeviceId t) = toJSON $ original t


newtype CallId =
  CallId Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON)


-- | Globally unique call ID.
newtype UCID =
  UCID Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON)


newtype Extension =
  Extension Int
  deriving (Num, Enum, Real, Integral,
            Eq, Ord, Show,
            Data, Typeable,
            FromJSON, ToJSON)


newtype SwitchName =
  SwitchName Text
  deriving (Eq, Ord, Show, Data, Typeable, FromJSON, ToJSON)


newtype AgentId =
  AgentId (SwitchName, Extension)
  deriving (Data, Typeable, Eq, Ord, Show, FromJSON, ToJSON)


data CallDirection = In { vdn :: DeviceId }
                   | Out
                   deriving (Eq, Show)


$(deriveJSON
  defaultOptions{sumEncoding = defaultTaggedObject{tagFieldName="dir"}}
  ''CallDirection)


data Call = Call
  { direction :: CallDirection
  , ucid :: UCID
  , start :: UTCTime
  -- ^ When did call came into existence?
  , interlocutors :: [DeviceId]
  , answered :: Maybe UTCTime
  -- ^ When did another party answer this call?
  , held :: Bool
  , failed :: Bool
  }
  deriving Show


$(deriveJSON defaultOptions ''Call)


data SettableAgentState = Ready
                        | AfterCall
                        | NotReady
                        | Logout
                        deriving (Eq, Show)


$(deriveJSON defaultOptions ''SettableAgentState)


data AgentState = Busy
                | Settable SettableAgentState
                deriving (Eq, Show)


instance ToJSON AgentState where
  toJSON Busy         = String "Busy"
  toJSON (Settable s) = toJSON s


instance FromJSON AgentState where
  parseJSON (String "Busy") = return Busy
  parseJSON s@(String _)    = Settable <$> parseJSON s
  parseJSON _               = fail "Could not parse AgentState"


data LoggingOptions = LoggingOptions
  { syslogIdent :: String
  }


data SessionOptions = SessionOptions
  { statePollingDelay :: Int
    -- ^ How often to poll every agent for state changes (in seconds).
  }
