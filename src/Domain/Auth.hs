module Domain.Auth (
 -- * Types 
 Auth(..), 
 Email, 
 mkEmail, 
 rawEmail, 
 Password, 
 mkPassword, 
 rawPassword, 
 UserId, 
 VerificationCode, 
 SessionId, 
 RegistrationError(..), 
 EmailVerificationError(..), 
 LoginError(..), 

 -- * Ports
 AuthRepo(..), 
 EmailVerificationNotif(..), 
 SessionRepo(..), 

 -- * Use Case
 register, 
 verifyEmail, 
 login, 
 resolveSessionId, 
 getUser

) where

import Katip
import ClassyPrelude
import Domain.Validation 
import Text.Regex.PCRE.Heavy 
import Control.Monad.Except 

data Auth = Auth 
        { authEmail :: Email
        , authPassword :: Password 
        } deriving (Show, Eq)

data RegistrationError
        = RegistrationErrorEmailTaken
        deriving (Show, Eq)

-- The reason to use newtype for email and password datatypes 
-- is so that the only way to make email and passwords is through the 
-- use of mkEmail/mkPassword functions 
-- this way at compile time we know any email/password crated is valid
-- pattern known as smart constructor 
newtype Email = Email {emailRaw :: Text } deriving (Show, Eq, Ord)
-- data EmailValidationErr = EmailValidationErrInvalidEmail 

rawEmail :: Email -> Text 
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email 
mkEmail = validate Email 
        [ regexMatches
                [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
                "Not a valid email"
        ]

newtype Password = Password {passwordRaw :: Text } deriving (Show, Eq)
{-data PasswordValidationErr
        = PasswordValidationErrLength Int 
        | PasswordValidationErrMustContainUpperCase
        | PasswordValidationErrMustContainLowerCase
        | PasswordValidationErrMustContainNumber 
-}
rawPassword :: Password -> Text 
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password 
mkPassword = validate Password 
        [ lengthBetween 5 50 "Should be between 5 and 50"
        , regexMatches [re|\d|] "Should contain number"
        , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
        , regexMatches [re|[a-z]|] "Should contain lowercase letter"
        ]

-- These actions have sideeffects, hence, monad 
type VerificationCode = Text 
data EmailVerificationError = EmailVerificationErrorInvalidCode 
        deriving (Show, Eq)

class Monad m => AuthRepo m where 
        addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
        findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
        setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
        findEmailFromUserId :: UserId -> m (Maybe Email)

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ()) 
verifyEmail vCode = runExceptT $ do 
        (uId, email) <- ExceptT $ setEmailAsVerified vCode
        withUserIdContext uId $ 
                $(logTM) (InfoS) $ ls (rawEmail email) <> " is verified successfully"
        return () 

class Monad m => EmailVerificationNotif m where 
        notifyEmailVerification :: Email -> VerificationCode -> m () 


withUserIdContext :: (KatipContext m) => UserId -> m a -> m a 
withUserIdContext uId = katipAddContext (sl "userId" uId)

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m) 
        => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do 
        (uId, vCode) <- ExceptT $ addAuth auth 
        let email = authEmail auth 
        lift $ notifyEmailVerification email vCode 
        withUserIdContext uId $ 
                $(logTM) (InfoS) $ ls (rawEmail email) <> " is registered successfully" 


-- Temporary IO instance of AuthRepo and EmailVerification typeclass
{-
instance AuthRepo IO where 
        addAuth (Auth email pass) = do 
                putStrLn $ "adding auth: " <> rawEmail email 
                return $ Right "fake verfication code"

instance EmailVerificationNotif IO where 
        notifyEmailVerification email vcode = 
                putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode 
-}

-- Login and Resolving Session 
type UserId = Int 
type SessionId = Text 
data LoginError = LoginErrorInvalidAuth
                | LoginErrorEmailNotVerified
                deriving (Show, Eq)

class Monad m => SessionRepo m where 
        newSession :: UserId -> m SessionId 
        findUserIdBySessionId :: SessionId -> m (Maybe UserId)

login :: (KatipContext m, AuthRepo m, SessionRepo m) 
        => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do 
        result <- lift $ findUserByAuth auth 
        case result of 
                Nothing -> throwError LoginErrorInvalidAuth 
                Just (_, False) -> throwError LoginErrorEmailNotVerified
                Just (uId, _) -> withUserIdContext uId . lift $ do 
                        sId <- newSession uId 
                        $(logTM) (InfoS) $ ls (rawEmail $ authEmail auth) <> " logged in successfully"
                        return sId 

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId 


