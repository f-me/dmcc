{-# LANGUAGE TemplateHaskell #-}

{-| Incoming XML messages (responses and events). -}

module CSTA.XML.Response
    ( Response(..)
    , Event(..)
    , fromXml
    )

where

import           Control.Exception (SomeException)
import           Data.ByteString.Lazy (ByteString)
import           Data.CaseInsensitive (mk)
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

import           Data.Aeson.TH

import           Text.XML
import           Text.XML.Cursor

import           CSTA.Types


data Response
  = UnknownResponse ByteString
  | MalformedResponse ByteString SomeException
  | StartApplicationSessionPosResponse
    {sessionID :: Text
    ,actualProtocolVersion :: Text
    ,actualSessionDuration :: Int
    }
  | GetDeviceIdResponse
    {device :: DeviceId
    }
  | GetThirdPartyDeviceIdResponse
    {device :: DeviceId
    }
  | MonitorStartResponse
    {monitorCrossRefID :: Text
    }
  | EventResponse
    {monitorCrossRefID :: Text
    ,event :: Event
    }
  deriving Show


data Event =
  UnknownEvent
  -- | Precedes every established/cleared event.
  | DeliveredEvent
    { callId :: CallId
    , callingDevice :: DeviceId
    , calledDevice :: DeviceId
    }
  | EstablishedEvent
    {callId :: CallId}
  | ConnectionClearedEvent
    { callId :: CallId
    , releasingDevice :: DeviceId
    }
  | HeldEvent
    {callId :: CallId}
  | RetrievedEvent
    {callId :: CallId}
  | ConferencedEvent
    { primaryOldCall :: CallId
    , secondaryOldCall :: CallId
    }
  deriving Show

$(deriveToJSON
  defaultOptions{sumEncoding = defaultTaggedObject{tagFieldName="event"}}
  ''Event)

fromXml :: ByteString -> Response
fromXml xml
  = case parseLBS def xml of
    Left err -> MalformedResponse xml err
    Right doc -> let cur = fromDocument doc
      in case nameLocalName $ elementName $ documentRoot doc of
        "StartApplicationSessionPosResponse"
          -> StartApplicationSessionPosResponse
            {sessionID = text cur "sessionID"
            ,actualProtocolVersion = text cur "actualProtocolVersion"
            ,actualSessionDuration = decimal cur "actualSessionDuration"
            }

        "GetDeviceIdResponse"
          -> GetDeviceIdResponse
            {device = DeviceId $ mk $ text cur "device"
            }

        "GetThirdPartyDeviceIdResponse"
          -> GetThirdPartyDeviceIdResponse
            {device = DeviceId $ mk $ text cur "device"
            }

        "MonitorStartResponse"
          -> MonitorStartResponse
            {monitorCrossRefID = text cur "monitorCrossRefID"
            }

        "DeliveredEvent"
          -> EventResponse (text cur "monitorCrossRefID") $
             DeliveredEvent
             { callId =
               CallId $ textFromPath cur "connection" ["callId"]
             , callingDevice =
               DeviceId $ mk $ textFromPath cur "callingDevice" ["deviceIdentifier"]
             , calledDevice =
               DeviceId $ mk $ textFromPath cur "calledDevice" ["deviceIdentifier"]
             }

        "EstablishedEvent"
          -> EventResponse (text cur "monitorCrossRefID") $
             EstablishedEvent
             { callId =
               CallId $ textFromPath cur "establishedConnection" ["callId"]
             }

        "HeldEvent"
          -> EventResponse (text cur "monitorCrossRefID") $
             HeldEvent
             { callId =
               CallId $ textFromPath cur "heldConnection" ["callId"]
             }

        "RetrievedEvent"
          -> EventResponse (text cur "monitorCrossRefID") $
             RetrievedEvent
             { callId =
               CallId $ textFromPath cur "retrievedConnection" ["callId"]
             }

        "ConferencedEvent"
          -> EventResponse (text cur "monitorCrossRefID") $
             ConferencedEvent
             { primaryOldCall =
               CallId $ textFromPath cur "primaryOldCall" ["callId"]
             , secondaryOldCall =
               CallId $ textFromPath cur "secondaryOldCall" ["callId"]
             }

        "ConnectionClearedEvent"
          -> EventResponse (text cur "monitorCrossRefID") $
             ConnectionClearedEvent
             { callId =
               CallId $ textFromPath cur "droppedConnection" ["callId"]
             , releasingDevice =
               DeviceId $ mk $
               textFromPath cur "releasingDevice" ["deviceIdentifier"]
             }

        _ -> UnknownResponse xml

text c n = textFromPath c n []

decimal c n = let txt = text c n
  in case T.decimal txt of
    Right (x,"") -> x
    _ -> error $ "Can't parse as decimal: " ++ show txt

textFromPath cur rootName extraNames =
  T.concat $
  cur $//
  ((foldl1' (&/) (Data.List.map laxElement (rootName:extraNames))) &/ content)
