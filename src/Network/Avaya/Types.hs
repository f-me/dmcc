module Network.Avaya.Types where

import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe
import           Network
import           System.IO

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Error
import           Control.Monad.State
import           Data.ByteString.Lex.Integral (packDecimal)
import qualified Data.Text as T


data AvayaException
    -- | The remote host refused the specified authentication
    -- credentials.
    = AuthenticationFailure

    -- | The sessionID given by the application is not valid or not
    -- known by the server.
    | InvalidSessionId

    -- | The session terminated due to a resource constraint.
    | ResourceLimitation


instance Error AvayaException where
    strMsg = error


data Event = Event
    { eventMonitor :: T.Text
    , eventDevice  :: T.Text
    , eventType    :: EventType
    } deriving (Show)


data EventType
    = Hookswitch  -- ^ the switch has changed the device's hookswitch status
        { hookswitchId     :: T.Text
        , hookswitchOnHook :: Bool
        }
    | DisplayUpdated
    | LampMode
    | RingerStatus
      deriving (Show)


data Conf = Conf
    { cHost         :: HostName    -- ^ AES host name
    , cPort         :: PortNumber  -- ^ AES port number - default port is 4721
    , cUser         :: T.Text
    , cUserPassword :: T.Text
    , cDelay        :: T.Text
    , cVersion      :: T.Text      -- ^ AES version
    , cDuration     :: T.Text      -- ^ amount of time in seconds that a session will remain active
    , cCallServerIp :: T.Text
    , cExtension    :: T.Text
    , cPassword     :: T.Text
    } deriving (Show)


data Session = Session
    { sIn         :: Maybe (TChan B.ByteString)
    , sOut        :: Maybe (TChan B.ByteString)
    , sInvokeId   :: Int
    , sSessionId  :: Maybe T.Text
    , sProtocol   :: Maybe T.Text
    , sDeviceId   :: Maybe T.Text
    , sMonitorId  :: Maybe T.Text
    }


defaultSession =
    Session
      { sIn         = Nothing
      , sOut        = Nothing
      , sInvokeId   = 0
      , sSessionId  = Nothing
      , sProtocol   = Nothing
      , sDeviceId   = Nothing
      , sMonitorId  = Nothing
      }


type Avaya a = ErrorT AvayaException (StateT Session IO) a


startAvaya input output avaya = fst <$> (runStateT (runErrorT avaya) $ defaultSession {sIn = Just input , sOut = Just output})


------------------------------------------------------------------------------
-- FIXME: Avoid boilerplate

getHandle = undefined

readChanA :: Avaya B.ByteString
readChanA = do
  c <- fromJust . sIn <$> get
  liftIO $ atomically $ readTChan c


writeChanA :: B.ByteString -> Avaya ()
writeChanA a = do
  c <- fromJust . sOut <$> get
  liftIO $ atomically $ writeTChan c a


getIn :: Avaya (TChan B.ByteString)
getIn = fromJust . sIn <$> get  -- FIXME

getOut :: Avaya (TChan B.ByteString)
getOut = fromJust . sOut <$> get

nextInvokeId :: Avaya B.ByteString
nextInvokeId = do
    s <- get
    let i = sInvokeId s
    put $ s { sInvokeId = increase i }
    return $ packInvokeId i
  where
    increase i = if i < 9998
                   then i + 1
                   else 0

    packInvokeId i = addNulls $ B.fromChunks [ fromJust $ packDecimal i ]

    addNulls x = B.replicate (4 - B.length x) '0' `B.append` x


getSessionId :: Avaya T.Text
getSessionId = fromJust . sSessionId <$> get


setSessionId :: T.Text -> Avaya ()
setSessionId id = do
    s <- get
    put $ s { sSessionId = Just id }


getProtocol :: Avaya T.Text
getProtocol = fromJust . sProtocol <$> get


setProtocol :: T.Text -> Avaya ()
setProtocol protocol = do
    s <- get
    put $ s { sProtocol = Just protocol }


getDeviceId :: Avaya T.Text
getDeviceId = fromJust . sDeviceId <$> get


setDeviceId :: T.Text -> Avaya ()
setDeviceId devId = do
    s <- get
    put $ s { sDeviceId = Just devId }


getMonitorId :: Avaya T.Text
getMonitorId = fromJust . sMonitorId <$> get


setMonitorId :: T.Text -> Avaya ()
setMonitorId monId = do
    s <- get
    put $ s { sMonitorId = Just monId }
