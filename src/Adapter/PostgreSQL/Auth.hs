module Adapter.PostgreSQL.Auth where 

import qualified Domain.Auth as D 
import Data.Has 
import Text.StringRandom
import ClassyPrelude
import Data.Pool 
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple 
import Data.Time 

type State = Pool Connection 
type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

data Config = Config 
        { configUrl :: ByteString
        , configStripeCount :: Int 
        , configMaxOpenConnPerStripe :: Int 
        , configIdleConnTimeout :: NominalDiffTime
        }

withConn :: PG r m 
                => (Connection -> IO a) -> m a
withConn action = do 
        pool <- asks getter 
        liftIO . withResource pool $ \conn -> action conn 

addAuth :: PG r m 
                => D.Auth 
                -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.Auth email pass) = do 
        let rawEmail = D.rawEmail email 
        let rawPass = D.rawPassword pass 
        vCode <- liftIO $ do 
                r <- stringRandomIO "[A-Za-z0-9]{16}"
                return $ (tshow rawEmail) <> "_" <> r 
        result <- withConn $ \conn -> 
                try $ query conn qry (rawEmail, rawPassw, vCode)
        case result of 
                Right [Only uId] -> return $ Right (uId, vCode)
                Right _ -> throwString "Should not happen: PG doesn't return userId"
                Left err@SqlError{sqlState = state, sqlErrorMsg = msg } -> 
                        if state == "23505" && "auths_email_key" `isInfixOf` msg 
                                then return $ Left D.RegistrationErrorEmailTaken 
                                else throwString $ "Unhandled PG exception: " <> show err
        where 
                qry = "insert into auth \
                        \(email, pass, email_verification_code, is_email_verified) \
                        \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"


withPool :: Config -> (State -> IO a) -> IO a 
withPool cfg action = 
        bracket initPool cleanPool action 
        where 
                initPool = createPool openConn closeConn 
                                (configStripeCount cfg)
                                (configIdleConnTimeout cfg)
                                (configMaxOpenConnPerStripe cfg)
                cleanPool = destroyAllResources
                openConn = connectPostgreSQL (configUrl cfg)
                closeConn = close 

withState :: Config -> (State -> IO a) -> IO a 
withState cfg action = 
        withPool cfg $ \state -> do 
                migrate state 
                action state 

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do 
        result <- withTransaction conn (runMigrations False conn cmds)
        case result of 
                MigrationError err -> throwString err
                _ -> return () 
        where
                cmds =  [ MigrationInitialization
                        , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
                        ]
