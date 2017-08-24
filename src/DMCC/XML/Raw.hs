{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|

DMCC request/response packet processing.


DMCC header format:

@
|  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  | ...        |
|  Version  |   Length  |       InvokeID        | XML Message Body |
@

-}

module DMCC.XML.Raw where

import           DMCC.Prelude

import           Control.Monad.Logger
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Lazy.Char8  as L8

import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import           Text.Printf

import           DMCC.XML.Request
import           DMCC.XML.Response


-- FIXME: error
sendRequest :: MonadLoggerIO m
            => OutputStream S.ByteString
            -> Int
            -> Request
            -> m ()
sendRequest h ix rq =
  let
    rawRequest = toXml rq
  in
    do
      logDebugN $
        "Sending request (invokeId=" <> tshow ix <> ") " <>
        tshow (L8.unpack rawRequest)
      liftIO $ flip Streams.writeLazyByteString h $ runPut $ do
        putWord16be 0
        putWord16be . fromIntegral $ 8 + L.length rawRequest
        let invokeId = S.pack . take 4 $ printf "%04d" ix
        putByteString invokeId
        putLazyByteString rawRequest


-- | Read a CSTA message from an input stream. Throws
-- 'ReadTooShortException'.
readResponse :: MonadLoggerIO m
             => InputStream S.ByteString
             -> m (Response, Int)
readResponse h = do
  -- xml-conduit parser requires a lazy ByteString
  let readLazy i = do
        v <- Streams.readExactly i h
        return $ L.fromChunks [v]
  (len, invokeId) <- liftIO $ runGet readHeader <$> readLazy 8
  resp <- liftIO $ readLazy (len - 8)
  logDebugN $
    "Received response (invokeId=" <> tshow invokeId <> ") " <>
    tshow (L8.unpack resp)
  return (fromXml resp, invokeId)
  where
    readHeader = do
      skip 2 -- version
      len <- fromIntegral <$> getWord16be
      ix  <- getByteString 4
      case S.readInt ix of
        Just (invokeId, "") -> return (len, invokeId)
        _ -> fail $ "Invalid InvokeID: " ++ show ix
