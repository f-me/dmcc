{-# LANGUAGE TemplateHaskell #-}

{-| Incoming XML messages (responses and events). -}

module DMCC.XML.Response
    ( Response (..)
    , Event (..)
    , fromXml
    )

where

import           DMCC.Prelude

import           Data.CaseInsensitive (mk)
import           Data.List (foldl1')
import qualified Data.Text as T
import qualified Data.Text.Read as T

import           Data.Aeson.TH

import           Text.XML
import           Text.XML.Cursor

import           DMCC.Types


-- | DMCC response to a request.
data Response
  = UnknownResponse LByteString
  | MalformedResponse LByteString SomeException
  | StartApplicationSessionPosResponse
    { sessionID :: Text
    , actualProtocolVersion :: Text
    , actualSessionDuration :: Int
    }
  | StartApplicationSessionNegResponse
  | GetDeviceIdResponse
    { device :: DeviceId
    }
  | GetThirdPartyDeviceIdResponse
    { device :: DeviceId
    }
  | MonitorStartResponse
    { monitorCrossRefID :: Text
    }
  | MakeCallResponse
    { newCallId :: CallId
    , newUcid :: UCID
    }
  | SingleStepConferenceCallResponse
    { conferencedCall :: CallId
    }
  | GetAgentStateResponse
    { agentState :: Maybe AgentState
      -- ^ Nothing if an unknown state is reported.
    , reasonCode :: Text
    }
  | GetCallLinkageDataResponse
    { linkageUcid :: UCID
    }
  | EventResponse
    { monitorCrossRefID :: Text
    , event :: Event
    }
  -- | Pretty-printed <CSTAErrorCode> message.
  | CSTAErrorCodeResponse
    { errorText :: Text
    }
  deriving Show


-- | DMCC event.
data Event =
  UnknownEvent
  -- | Precedes every established/cleared event.
  | DeliveredEvent
    { callId :: CallId
    , distributingVdn :: DeviceId
    , ucid :: UCID
    , callingDevice :: DeviceId
    , calledDevice :: DeviceId
    }
  | DivertedEvent
    { callId :: CallId
    }
  | OriginatedEvent
    { callId :: CallId
    , callingDevice :: DeviceId
    , calledDevice :: DeviceId
    }
  | EstablishedEvent
    { callId :: CallId
    }
  | FailedEvent
    { callId :: CallId
    }
  | ConnectionClearedEvent
    { callId :: CallId
    , releasingDevice :: DeviceId
    }
  | HeldEvent
    { callId :: CallId
    }
  | RetrievedEvent
    { callId :: CallId
    }
  | ConferencedEvent
    { primaryOldCall :: CallId
    , secondaryOldCall :: CallId
    }
  | TransferedEvent
    { primaryOldCall :: CallId
    , secondaryOldCall :: CallId
    }
  deriving Show

$(deriveJSON
  defaultOptions{sumEncoding = defaultTaggedObject{tagFieldName="event"}}
  ''Event)

fromXml :: LByteString -> Response
fromXml xml
  = case parseLBS def xml of
    Left err -> MalformedResponse xml err
    Right doc -> let cur = fromDocument doc
                     evResp = EventResponse (text cur "monitorCrossRefID")
      in case nameLocalName $ elementName $ documentRoot doc of
        "StartApplicationSessionPosResponse" ->
          StartApplicationSessionPosResponse
          { sessionID = text cur "sessionID"
          , actualProtocolVersion = text cur "actualProtocolVersion"
          , actualSessionDuration = decimal cur "actualSessionDuration"
          }

        "StartApplicationSessionNegResponse" ->
          StartApplicationSessionNegResponse

        "GetDeviceIdResponse" ->
          GetDeviceIdResponse
          { device = DeviceId $ mk $ text cur "device"
          }

        "GetThirdPartyDeviceIdResponse" ->
          GetThirdPartyDeviceIdResponse
          { device = DeviceId $ mk $ text cur "device"
          }

        "MonitorStartResponse" ->
          MonitorStartResponse
          {monitorCrossRefID = text cur "monitorCrossRefID"
          }

        "MakeCallResponse" ->
          MakeCallResponse
          { newCallId = CallId $ textFromPath cur "callingDevice" ["callId"]
          , newUcid =
            UCID $ text cur "globallyUniqueCallLinkageID"
          }

        "SingleStepConferenceCallResponse" ->
          SingleStepConferenceCallResponse
          { conferencedCall =
            CallId $ textFromPath cur "conferencedCall" ["callId"]
          }

        "GetAgentStateResponse" ->
          GetAgentStateResponse
          { agentState =
            let
                raw = textFromPath cur "agentStateList"
                      [ "agentStateEntry"
                      , "agentInfo"
                      , "agentInfoItem"
                      , "agentState"
                      ]
                state | raw == "agentReady" =
                          Just $ Settable Ready
                      | raw == "agentWorkingAfterCall" =
                          Just $ Settable AfterCall
                      | raw == "agentNotReady" =
                          Just $ Settable NotReady
                      | raw == "agentBusy" =
                          Just Busy
                      | otherwise = Nothing
            in
              state
          , reasonCode = textFromPath cur "extensions"
                         [ "privateData"
                         , "private"
                         , "GetAgentStateResponsePrivateData"
                         , "reasonCode"
                         ]
          }

        "GetCallLinkageDataResponse" ->
          GetCallLinkageDataResponse
          { linkageUcid = UCID $ text cur "globallyUniqueCallLinkageID"
          }

        "DeliveredEvent" -> evResp
          DeliveredEvent
          { callId =
            CallId $ textFromPath cur "connection" ["callId"]
          , distributingVdn =
            DeviceId $ mk $
            textFromPath cur "distributingVDN" ["deviceIdentifier"]
          , ucid =
            UCID $ text cur "globallyUniqueCallLinkageID"
          , callingDevice =
            DeviceId $ mk $ textFromPath cur "callingDevice" ["deviceIdentifier"]
          , calledDevice =
            DeviceId $ mk $ textFromPath cur "calledDevice" ["deviceIdentifier"]
          }

        "OriginatedEvent" -> evResp
          OriginatedEvent
          { callId =
            CallId $ textFromPath cur "originatedConnection" ["callId"]
          , callingDevice =
            DeviceId $ mk $ textFromPath cur "callingDevice" ["deviceIdentifier"]
          , calledDevice =
            DeviceId $ mk $ textFromPath cur "calledDevice" ["deviceIdentifier"]
          }

        "DivertedEvent" -> evResp
          DivertedEvent
          { callId =
            CallId $ textFromPath cur "connection" ["callID"]
          }

        "EstablishedEvent" -> evResp
          EstablishedEvent
          { callId =
            CallId $ textFromPath cur "establishedConnection" ["callId"]
          }

        "FailedEvent" -> evResp
          FailedEvent
          { callId =
            CallId $ textFromPath cur "failedConnection" ["callId"]
          }

        "HeldEvent" -> evResp
          HeldEvent
          { callId =
            CallId $ textFromPath cur "heldConnection" ["callId"]
          }

        "RetrievedEvent" -> evResp
          RetrievedEvent
          { callId =
            CallId $ textFromPath cur "retrievedConnection" ["callId"]
          }

        "ConferencedEvent" -> evResp
          ConferencedEvent
          { primaryOldCall =
            CallId $ textFromPath cur "primaryOldCall" ["callId"]
          , secondaryOldCall =
            CallId $ textFromPath cur "secondaryOldCall" ["callId"]
          }

        "TransferedEvent" -> evResp
          TransferedEvent
          { primaryOldCall =
            CallId $ textFromPath cur "primaryOldCall" ["callId"]
          , secondaryOldCall =
            CallId $ textFromPath cur "secondaryOldCall" ["callId"]
          }

        "ConnectionClearedEvent" -> evResp
          ConnectionClearedEvent
          { callId =
            CallId $ textFromPath cur "droppedConnection" ["callId"]
          , releasingDevice =
            DeviceId $ mk $
            textFromPath cur "releasingDevice" ["deviceIdentifier"]
          }

        "CSTAErrorCode" ->
          CSTAErrorCodeResponse
          { errorText =
            let
              msg = T.concat $ cur $// content
              err = case map node (cur $/ checkElement (const True)) of
                      (NodeElement el:_) ->
                        nameLocalName $ elementName el
                      _ -> "CSTAErrorCode"
            in
              T.concat [err, "/", msg]
          }

        _ -> UnknownResponse xml


text :: Cursor -> Text -> Text
text c n = textFromPath c n []


decimal :: Cursor -> Text -> Int
decimal c n =
  case T.decimal txt of
    Right (x, "") -> x
    _ -> error $ "Can't parse as decimal: " <> show txt
  where
    txt = text c n :: Text


-- | Extract contents of the first element which matches provided
-- path (@rootName:extraNames@). Return empty text if no element
-- matches the path.
textFromPath :: Cursor -> Text -> [Text] -> Text
textFromPath cur rootName extraNames =
  fromMaybe "" $ headMay contents
  where
    contents =
      cur $// foldl1' (&/) (map laxElement $ rootName : extraNames) &/ content
