{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Shared type declarations. -}

module CSTA.Types

where

import Data.Aeson
import Data.Aeson.TH
import Data.Data
import Data.Text
import Data.Time.Clock


newtype DeviceId =
  DeviceId Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON)


newtype CallId =
  CallId Text
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
  deriving (Data, Typeable, Eq, Ord, Show)


data CallDirection =
  In | Out
  deriving (Eq, Show)


$(deriveToJSON defaultOptions ''CallDirection)


data Call = Call
  { direction :: CallDirection
  , start :: UTCTime
  , interlocutor :: DeviceId
  , answered :: Maybe UTCTime
  }
  deriving Show


$(deriveToJSON defaultOptions ''Call)
