module Adapter.Redis.Auth where 

import ClassyPrelude
import qualified Domain.Auth as D
import Text.StringRandom
import Data.Has 
import qualified Database.Redis as R
import qualified Control.Monad.Catch as CCM

type State = R.Connection 

-- format => redis://user:pass@host:port/db 
withState :: String -> (State -> IO a) -> IO a
withState connUrl action = do 
        case R.parseConnectInfo connUrl of 
                Left _ -> 
                        throwString "Invalid Redis conn URL"
                Right connInfo -> do 
                        conn <- R.checkedConnect connInfo 
                        action conn 

type Redis r m = (Has State r, MonadReader r m, MonadIO m, CCM.MonadThrow m)

withConn :: Redis r m => R.Redis a -> m a
withConn action = do 
        conn <- asks getter
        liftIO $ R.runRedis conn action 

newSession :: Redis r m => D.UserId -> m D.SessionId 
newSession userId = do 
        sId <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
        result <- withConn $ R.set (encodeUtf8 sId) (fromString . show $ userId)
        case result of 
                Right R.Ok -> return sId
                err -> throwString $ "Unexpected redis error: " <> show err

findUserBySessionId :: Redis r m => D.SessionId -> m (Maybe D.UserId)
findUserBySessionId sId = do 
        result <- withConn $ R.get (encodeUtf8 sId)
        return $ case result of 
                        Right (Just uIdStr) -> readMay . unpack . decodeUtf8 $ uIdStr
                        err -> readMay ['e'] --ehrowString $ "Unexpected redis error: " <> show err 


