
module Avaya.Messages.Response where


import Control.Exception (SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Text.XML
import Text.XML.Cursor


data Response
  = UnknownResponse ByteString
  | MalformedResponse ByteString SomeException
  | StartApplicationSessionPosResponse
    {sessionID :: Text
    ,actualProtocolVersion :: Text
    ,actualSessionDuration :: Int
    }
  | GetDeviceIdResponse
    {device :: Text
    }
  | MonitorStartResponse
  | RegisterTerminalResponse
  deriving Show

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
            {device = text cur "device"
            }

        "MonitorStartResponse" -> MonitorStartResponse
        "RegisterTerminalResponse" -> RegisterTerminalResponse

        _ -> UnknownResponse xml

text c n = T.concat $ c $// laxElement n &/content
decimal c n = let txt = text c n
  in case T.decimal txt of
    Right (x,"") -> x
    _ -> error $ "Can't parse as decimal: " ++ show txt
