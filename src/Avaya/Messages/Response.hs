
module Avaya.Messages.Response where


import Data.ByteString (ByteString)
import qualified Data.ByteString as S


data Response
  = Rs1
  | UnknownResponse ByteString
  | MalformedResponse String
  deriving Show

fromXml :: ByteString -> Response
fromXml = UnknownResponse
