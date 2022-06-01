module Lib
  ( someFunc
  ) where

import Katip 
import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import Domain.Auth
import Control.Monad (MonadFail) 
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Control.Monad.Catch as CCM
import qualified Adapter.Redis.Auth as Redis 

type State = (PG.State, Redis.State, TVar M.State)

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
        notifyEmailVerification = M.notifyEmailVerification

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


someFunc :: IO ()
someFunc = withKatip $ \le -> do
        mState <- newTVarIO M.initialState
        PG.withState pgCfg $ \pgState -> 
                Redis.withState redisCfg $ \redisState -> 
                        run le (pgState, redisState, mState) action 
        where 
                redisCfg = "redis://172.17.0.3:6379/0"
                pgCfg = PG.Config 
                        { PG.configUrl = "postgresql://postgres:postgres@172.17.0.2:5432/hauth"
                        , PG.configStripeCount = 2
                        , PG.configMaxOpenConnPerStripe = 5
                        , PG.configIdleConnTimeout = 10
                        }

action :: App ()
action = do
        let email = either undefined id $ mkEmail "ecky@test.com"
        let passw = either undefined id $ mkPassword "1234ABCDefgh"
        let auth = Auth email passw
        register auth
        Just vCode <- M.getNotificationsForEmail email
        verifyEmail vCode
        Right session <- login auth
        Just uId <- resolveSessionId session
        Just registeredEmail <- getUser uId
        print (session, uId, registeredEmail)


runKatip :: IO () 
runKatip = withKatip $ \le -> 
        runKatipContextT le () mempty logSomething 


logSomething :: (KatipContext m) => m () 
logSomething = do 
        $(logTM) InfoS "Log in no namespace"
        katipAddNamespace "ns1" $ 
                $(logTM) InfoS "Log in ns1"
        katipAddNamespace "ns2" $ do 
                $(logTM) WarningS "Log in ns2"
                katipAddNamespace "ns3" $ 
                        katipAddContext (sl "userId" $ asText "12") $ do 
                                $(logTM) InfoS "Log in ns2.ns3 with userId context"
                                katipAddContext (sl "country" $ asText "Singapore") $
                                        $(logTM) InfoS "Log in ns2.ns3 with userId and country context"

