module Lib
  ( main  
  ) where

import Katip 
import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import Domain.Auth
import Control.Monad (MonadFail) 
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Control.Monad.Catch as CCM
import qualified Adapter.Redis.Auth as Redis 
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth as MQAuth 
import Text.StringRandom
import qualified Adapter.HTTP.Main as HTTP 

type State = (PG.State, Redis.State, MQ.State, TVar M.State)

newtype App a = App
        { unApp :: ReaderT State (KatipContextT IO) a
        } deriving      (Applicative
                        , Functor
                        , Monad
                        , MonadReader State
                        , MonadIO
                        , MonadFail
                        , KatipContext
                        , Katip 
                        , CCM.MonadThrow
                        , CCM.MonadCatch
                        , MonadUnliftIO
                        )

run :: LogEnv -> State -> App a -> IO a
run le state = 
        runKatipContextT le () mempty . flip runReaderT state . unApp

instance AuthRepo App where
        addAuth = PG.addAuth --M.addAuth
        setEmailAsVerified = PG.setEmailAsVerified --M.setEmailAsVerified
        findUserByAuth = PG.findUserByAuth--M.findUserByAuth
        findEmailFromUserId = PG.findEmailFromUserId--M.findEmailFromUserId

instance EmailVerificationNotif App where
        notifyEmailVerification = MQAuth.notifyEmailVerification -- M.notifyEmailVerification

instance SessionRepo App where
        newSession = Redis.newSession -- M.newSession
        findUserIdBySessionId = Redis.findUserBySessionId -- M.findUserIdBySessionId

withKatip :: (LogEnv -> IO a) -> IO a 
withKatip app = 
        bracket createLogEnv closeScribes app 
        where 
                createLogEnv = do 
                        logEnv <- initLogEnv "HAuth" "dev"
                        stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
                        registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv 

withState :: (Int -> LogEnv -> State -> IO ()) -> IO ()
withState action = 
        withKatip $ \le -> do 
                mState <- newTVarIO M.initialState
                PG.withState pgCfg $ \pgState -> 
                        Redis.withState redisCfg $ \redisState -> 
                                MQ.withState mqCfg 16 $ \mqState -> do 
                                        let state = (pgState, redisState, mqState, mState)
                                        action port le state 
        where 
                mqCfg = "amqp://guest:guest@172.17.0.4:5672/%2F"
                redisCfg = "redis://172.17.0.3:6379/0"
                pgCfg = PG.Config 
                        { PG.configUrl = "postgresql://postgres:postgres@172.17.0.2:5432/hauth"
                        , PG.configStripeCount = 2
                        , PG.configMaxOpenConnPerStripe = 5
                        , PG.configIdleConnTimeout = 10
                        }
                port = 3000

action :: App ()
action = do
        randEmail <- liftIO $ stringRandomIO "[a-z0-9]{5}@test\\.com"
        let email = either undefined id $ mkEmail randEmail -- "ecky@test.com"
        let passw = either undefined id $ mkPassword "1234ABCDefgh"
        let auth = Auth email passw
        register auth
        vCode <-pollNotif email
        verifyEmail vCode
        Right session <- login auth
        Just uId <- resolveSessionId session
        Just registeredEmail <- getUser uId
        print (session, uId, registeredEmail)
        where 
                pollNotif email = do
                        result <- M.getNotificationsForEmail email 
                        case result of 
                                Nothing -> pollNotif email
                                Just vCode -> return vCode 


runKatip :: IO () 
runKatip = withKatip $ \le -> 
        runKatipContextT le () mempty logSomething 


logSomething :: (KatipContext m) => m () 
logSomething = do 
        $(logTM) (InfoS) "Log in no namespace"
        katipAddNamespace "ns1" $ 
                $(logTM) (InfoS) "Log in ns1"
        katipAddNamespace "ns2" $ do 
                $(logTM) WarningS "Log in ns2"
                katipAddNamespace "ns3" $ 
                        katipAddContext (sl "userId" $ asText "12") $ do 
                                $(logTM) (InfoS) "Log in ns2.ns3 with userId context"
                                katipAddContext (sl "country" $ asText "Singapore") $
                                        $(logTM) (InfoS) "Log in ns2.ns3 with userId and country context"

main :: IO () 
main = 
        withState $ \port le state@(_,_,mqState,_) -> do 
                let runner = run le state
                MQAuth.init mqState runner 
                --runner action 
                HTTP.main port runner 
