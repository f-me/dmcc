
module Avaya.Messages.Response where


import Data.ByteString (ByteString)


data Response
  = UnknownResponse ByteString
  deriving Show

fromXml :: ByteString -> Response
fromXml = UnknownResponse
