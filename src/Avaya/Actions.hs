
module Avaya.Actions
  (dialNumber
  ) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

import Avaya.MessageLoop
import qualified Avaya.Messages.Request as Rq


dialNumber :: LoopHandle -> Text -> Text -> String -> IO ()
dialNumber h protocol device number = do
  forM_ number $ \n -> sendRequestSync h
    $ Rq.ButtonPress
      {acceptedProtocol = protocol
      ,device = device
      ,button = T.singleton n
      }
