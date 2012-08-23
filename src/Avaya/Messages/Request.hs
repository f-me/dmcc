
module Avaya.Messages.Request where


import Data.ByteString (ByteString)
import qualified Data.ByteString as S


data Request = Rq1 | Rq2
  deriving Show

toXml :: Request -> ByteString
toXml = undefined
