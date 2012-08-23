
module Avaya.Messages.Raw where


import System.IO
import Control.Exception
import Data.Functor ((<$>))
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy  as L
import Text.Printf

import Avaya.Messages.Request
import Avaya.Messages.Response


-- CSTA header format.
-- |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  | ...        |
-- |  Version  |   Length  |       InvokeID        | XML Message Body |


-- FIXME: error
sendRequest :: Handle -> Int -> Request -> IO ()
sendRequest h ix rq
  = L.hPut h $ runPut $ do
    putWord16be 0
    let rawRequest = toXml rq
    putWord16be . fromIntegral $ 8 + S.length rawRequest
    let invokeId = S.pack . take 4 $ printf "%04d" ix
    putByteString invokeId
    putByteString rawRequest


-- FIXME: error
readResponse :: Handle -> IO Response
readResponse h = do
  dataLen <- try $ runGet readHeader <$> L.hGet h 8
  case dataLen of
    Left err -> return $! MalformedResponse $! "Header: " ++ show (err :: SomeException)
    Right len -> fromXml <$> S.hGet h len
  where
    readHeader = do
      skip 2 -- version
      len <- fromIntegral <$> getWord16be
      ix  <- getByteString 4
      case S.readInt ix of
        Just (9999,"") -> return len
        _ -> fail $ "Invalid InvokeID: " ++ show ix
