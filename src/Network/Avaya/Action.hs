{-# LANGUAGE OverloadedStrings #-}
module Network.Avaya.Action
    ( call
    ) where
import Control.Applicative
import Control.Monad

import qualified Data.Text as T

import Network.Avaya.Internal
import Network.Avaya.Messages

call :: String    -- ^ telephone number
     -> Avaya ()
call ns = do
    setHookswitchStatus False
    mapM_ (buttonPress . T.singleton) ns


buttonPress :: T.Text -> Avaya ()
buttonPress button = do
    mess <- buttonPressMessage button <$> maccess sProtocol <*> maccess sDeviceId

    avayaWrite mess
    void avayaRead


setHookswitchStatus :: Bool -> Avaya ()
setHookswitchStatus isOnHook = do
    mess <- setHookswitchStatusMessage onHook
            <$> maccess sProtocol <*> maccess sDeviceId

    avayaWrite mess
    void avayaRead
  where
    onHook = if isOnHook then "true" else "false"


