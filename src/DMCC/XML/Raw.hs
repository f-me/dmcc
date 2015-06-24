{-|

DMCC request/response packet processing.


DMCC header format:

@
|  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  | ...        |
|  Version  |   Length  |       InvokeID        | XML Message Body |
@

-}

module DMCC.XML.Raw where


import           Data.Functor ((<$>))
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Lazy.Char8  as L8

import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import           System.Posix.Syslog

import           Text.Printf

import           DMCC.Types
import           DMCC.XML.Request
import           DMCC.XML.Response


maybeSyslog :: Maybe LoggingOptions -> Priority -> String -> IO ()
maybeSyslog Nothing _ _ = return ()
maybeSyslog (Just LoggingOptions{..}) pri msg =
  withSyslog syslogIdent [PID] USER (logUpTo Debug) $ syslog pri msg


-- FIXME: error
sendRequest :: Maybe LoggingOptions
            -> OutputStream S.ByteString
            -> Int
            -> Request
            -> IO ()
sendRequest lopts h ix rq =
  let
    rawRequest = toXml rq
  in
    do
      maybeSyslog lopts Debug $
        "Sending request (invokeId=" ++ show ix ++ ") " ++
        (show $ L8.unpack rawRequest)
      flip Streams.writeLazyByteString h $ runPut $ do
        putWord16be 0
        putWord16be . fromIntegral $ 8 + L.length rawRequest
        let invokeId = S.pack . take 4 $ printf "%04d" ix
        putByteString invokeId
        putLazyByteString rawRequest


-- | Read a CSTA message from an input stream. Throws
-- 'ReadTooShortException'.
readResponse :: Maybe LoggingOptions
             -> InputStream S.ByteString
             -> IO (Response, Int)
readResponse lopts h = do
  -- xml-conduit parser requires a lazy ByteString
  let readLazy i = do
        v <- Streams.readExactly i h
        return $ L.fromChunks [v]
  (len, invokeId) <- runGet readHeader <$> readLazy 8
  resp <- readLazy (len - 8)
  maybeSyslog lopts Debug $
    "Received response (invokeId=" ++ show invokeId ++ ") " ++
    (show $ L8.unpack resp)
  return (fromXml resp, invokeId)
  where
    readHeader = do
      skip 2 -- version
      len <- fromIntegral <$> getWord16be
      ix  <- getByteString 4
      case S.readInt ix of
        Just (invokeId, "") -> return (len, invokeId)
        _ -> fail $ "Invalid InvokeID: " ++ show ix
