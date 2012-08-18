{-# LANGUAGE OverloadedStrings #-}

module Network.Avaya.Parse where

import           Control.Applicative
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import           Data.Word

import           Text.XML
import           Text.XML.Cursor

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

readDecimal_ s = case BS.readInt s of
  Just (n,"") -> n
  _ -> error $ "can't parse " ++ show s ++ " as decimal"

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
