module Adapter.PostgreSQL.Auth where 

import qualified Domain.Auth as D 
import Data.Has 
import Text.StringRandom
import ClassyPrelude
import Data.Pool 
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple 
import Data.Time 
import qualified Control.Monad.Catch as CCM

type State = Pool Connection 
type PG r m = (Has State r, MonadReader r m, MonadIO m, CCM.MonadThrow m)

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
                try $ query conn qry (rawEmail, rawPass, vCode)
        case result of 
                Right [Only uId] -> return $ Right (uId, vCode)
                Right _ -> throwString "Should not happen: PG doesn't return userId"
                Left err@SqlError{sqlState = state, sqlErrorMsg = msg } -> 
                        -- 23505 is a unique constraint violation
                        if state == "23505" && "auths_email_key" `isInfixOf` msg 
                                then return $ Left D.RegistrationErrorEmailTaken 
                                else throwString $ "Unhandled PG exception: " <> show err
        where 
                qry = "insert into auth \
                        \(email, pass, email_verification_code, is_email_verified) \
                        \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"

setEmailAsVerified :: PG r m 
                        => D.VerificationCode
                        -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do 
        result <- withConn $ \conn -> query conn qry (Only vCode)
        case result of 
                -- query return 1 row 
                [(uId, mail)] -> case D.mkEmail mail of 
                                        Right email -> return $ Right (uId, email)
                                        _ -> throwString $ "Should not happen: email in DB is not valid: " <> unpack mail 
                -- query return o or 2+ rows 
                _ -> return $ Left D.EmailVerificationErrorInvalidCode 


        where
                qry = "update auths set is_email_verified = 't' \
                        \where email_verification_code = ? \
                        \returning id, cast (email as text)"

findUserByAuth :: PG r m 
                        => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth (D.Auth email pass) = do 
        let rawEmail = D.rawEmail email 
        let rawPass = D.rawPassword pass 
        result <- withConn $ \conn -> query conn qry (rawEmail, rawPass)
        return $ case result of 
                [(uId, isVerified)] -> Just (uId, isVerified)
                _ -> Nothing 
        where 
                qry = "select id, is_email_verified from auths where email = ? and pass = crypt(?, pass)"

findEmailFromUserId :: PG r m 
                        => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do 
        result <- withConn $ \conn -> query conn qry (Only uId)
        case result of 
                [Only mail] -> case D.mkEmail mail of 
                                        Right email -> return $ Just email 
                                        _ -> throwString $ "Should not happen: email in DB is not valid " <> unpack mail 
                _ -> 
                        return Nothing 
        where 
                qry = "select cast(email as text) from auths where id = ?"

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
