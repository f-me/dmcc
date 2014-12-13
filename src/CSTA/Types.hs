{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Shared type declarations. -}

module CSTA.Types

where

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Data.Time.Clock


newtype DeviceId =
  DeviceId Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON)


newtype CallId =
  CallId Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON)


newtype SwitchName =
  SwitchName Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON)


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
