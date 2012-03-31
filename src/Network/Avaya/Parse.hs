{-# LANGUAGE OverloadedStrings #-}

module Network.Avaya.Parse where

import           Control.Applicative
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Word

import           Data.ByteString.Lex.Integral (packDecimal, readDecimal_)
import           Text.XML
import           Text.XML.Cursor

--import           Network.Avaya.Events
import           Network.Avaya.Types
------------------------------------------------------------------------------
-- Parse header.
-- | All XML messages must have prepended the appropriate eight byte
-- header as specified by CSTA.
-- |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  | ...        |
-- |  Version  |   Length  |       InvokeID        | XML Message Body |
data CstaHeader = CstaHeader {
      hVersion  :: Word16
    , hLength   :: Int
    , hInvokeId :: Int      -- ^ request and response XML messages must use Invoke IDs between 0 and 9998
    }


putHeader :: B.ByteString -> B.ByteString -> Put
putHeader request invokeId = do
    putWord16be 0
    putWord16be . fromIntegral $ 8 + B.length request
    putLazyByteString invokeId
    putLazyByteString request


getHeader :: Get CstaHeader
getHeader = do
    version  <- getWord16be
    length   <- fromIntegral <$> getWord16be
    invokeId <- readDecimal_ <$> getByteString 4
    return $ CstaHeader version (length - 8) invokeId

------------------------------------------------------------------------------
parseEvent :: B.ByteString -> Event
parseEvent = event . parseResponse


event :: Document -> Event
event doc = Event { eventMonitor = el doc "monitorCrossRefID"
                  , eventDevice  = el doc "device"
                  , eventType    = parseEventType doc
                  }


parseEventType :: Document -> EventType
parseEventType doc =
    case nameLocalName (elementName $ documentRoot doc) of
      "HookswitchEvent"     ->
          Hookswitch { hookswitchId = el doc "hookswitch"
                     , hookswitchOnHook = parseBoolean $ el doc "hookswitchOnHook"
                     }
        where
      "LampModeEvent"       -> LampMode
      "RingerStatusEvent"   -> RingerStatus
      "DisplayUpdatedEvent" -> DisplayUpdated
      _                     -> error "parseEventType: no such event type"


parseBoolean b =
    case b of
      "true"  -> True
      "1"     -> True
      "false" -> False
      "0"     -> False
      other   -> error "parseBoolean: not boolean"


isHookswitch res = is res "HookswitchEvent"
isLampMode res = is res "LampModeEvent"

is res str = bool $ fromDocument (parseResponse res) $| laxElement str
------------------------------------------------------------------------------
-- Parse message.
parseResponse :: B.ByteString -> Document
parseResponse = parseLBS_ def  -- FIXME: maybe we should use parseLBS that returns Either SomeException Document?


isPosResponse res =
    bool $ fromDocument doc $| laxElement "StartApplicationSessionPosResponse"
  where
    doc = parseResponse res


parseSessionId  = sessionId . parseResponse
parseProtocol   = protocol . parseResponse
parseDeviceId   = deviceId  . parseResponse
parseMonitorId  = monitorId . parseResponse


sessionId doc = el doc "sessionID"
protocol  doc = el doc "actualProtocolVersion"
deviceId  doc = el doc "device"
monitorId doc = el doc "monitorCrossRefID"


el doc name =
    -- TEST: $// instead of $/
    head $ fromDocument doc $//  laxElement name  -- FIXME: head on empty list
                            &// content
