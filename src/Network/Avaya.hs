module Network.Avaya
    ( Avaya
    , AvayaException (..)
    , AvayaConfig (..)
    , AvayaState
    , startAvaya
    , runAvayaAction
    , cleanup
    ) where
import           Control.Arrow
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           System.IO

import           Control.Concurrent.STM
import           Network

import           Network.Avaya.Internal
import           Network.Avaya.Messages
import           Network.Avaya.Parse

startAvaya :: AvayaConfig -> IO (Either AvayaException AvayaState)
startAvaya conf = withSocketsDo $ do
    h <- connectTo host (PortNumber port)
    hSetBuffering h LineBuffering

    input  <- newTChanIO
    output <- newTChanIO

    forkIO $ loopRead h input
    forkIO $ loopWrite h output 0

    let as = AS (conf, defaultSession { _sIn = Just input, _sOut = Just output })

    res <- runAvayaAction as $ do
        startAppSession
        requestDeviceId
        monitorStart
        registerTerminal

    return $ right fst res
  where
    host = cHost conf
    port = cPort conf

-- Establish an application session.
startAppSession :: Avaya ()
startAppSession = do
    conf <- ask
    avayaWrite $ startAppSessionMessage conf
    res <- avayaRead

    sSessionId ~!= parseSessionId res
    void $ sProtocol ~!= parseProtocol res


requestDeviceId :: Avaya ()
requestDeviceId = do
    conf <- ask
    avayaWrite $ getDeviceIdMessage conf
    res <- avayaRead

    void $ sDeviceId ~!= parseDeviceId res


-- Request notification on events.
monitorStart :: Avaya ()
monitorStart = do
    mess <- monitorStartMessage
            <$> maccess sProtocol <*> maccess sDeviceId

    avayaWrite mess
    res <- avayaRead

    void $ sMonitorId ~!= parseMonitorId res


registerTerminal :: Avaya ()
registerTerminal = do
    conf <- ask
    mess <- registerTerminalRequestMessage conf
            <$> maccess sDeviceId

    avayaWrite mess
    void avayaRead


cleanup :: Avaya ()
cleanup = do
    -- Stop monitoring the events.
    monId <- maccess sMonitorId
    avayaWrite $ monitorStopMessage monId
    void avayaRead

    -- Release the device identifier.
    devId <- maccess sDeviceId
    avayaWrite $ releaseDeviceIdMessage devId
    void avayaRead

    -- Close the app session.
    sId <- maccess sSessionId
    avayaWrite $ stopAppSession sId
    void avayaRead



