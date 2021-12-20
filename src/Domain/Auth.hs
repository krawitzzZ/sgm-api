module Domain.Auth
  ( AuthenticatedUser(..)
  , JWT(..)
  , mkJwt
  , mkAuthenticatedUser
  ) where

import           Control.Exception.Safe                   ( MonadThrow
                                                          , throwM
                                                          )
import           Control.Monad.Reader.Has                 ( Has
                                                          , extract
                                                          )
import           Control.Monad.Time                       ( MonadTime(..) )
import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          , genericParseJSON
                                                          , genericToJSON
                                                          )
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.String.Conversions                  ( cs )
import           Data.UUID                                ( UUID )
import           Domain.Class                             ( Entity(..) )
import           Domain.Exception                         ( DomainException(..) )
import           Domain.User                              ( User(..) )
import           RIO                                      ( ($)
                                                          , (*)
                                                          , (<>)
                                                          , (>>=)
                                                          , Either(..)
                                                          , Eq
                                                          , Generic
                                                          , Maybe(..)
                                                          , MonadIO
                                                          , MonadReader
                                                          , Show
                                                          , Text
                                                          , asks
                                                          , liftIO
                                                          , return
                                                          , show
                                                          )
import           RIO.Time                                 ( UTCTime
                                                          , addUTCTime
                                                          , nominalDay
                                                          , secondsToNominalDiffTime
                                                          )
import           Servant.Auth.JWT                         ( FromJWT
                                                          , ToJWT
                                                          )
import           Servant.Auth.Server                      ( JWTSettings
                                                          , makeJWT
                                                          )
import           Utils                                    ( jsonOptions )


-- TODO add role
data AuthenticatedUser = AuthenticatedUser
  { auId   :: !UUID
  , auName :: !Text
  }
  deriving (Eq, Show, Generic)

instance Entity AuthenticatedUser where
  entityId = auId

instance ToJSON AuthenticatedUser where
  toJSON = genericToJSON $ jsonOptions "au"

instance FromJSON AuthenticatedUser where
  parseJSON = genericParseJSON $ jsonOptions "au"

instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

mkAuthenticatedUser :: User -> AuthenticatedUser
mkAuthenticatedUser User { uId = id, uUsername = username } =
  AuthenticatedUser { auId = id, auName = username }

data JWT = JWT
  { jwtAccessToken  :: ByteString
  , jwtRefreshToken :: ByteString
  }
  deriving (Eq, Show, Generic)

mkJwt :: (Has JWTSettings e, MonadReader e m, MonadTime m, MonadIO m) => AuthenticatedUser -> m JWT
mkJwt user = do
  jwtSettings                                 <- asks extract
  (accessTokenExpireAt, refreshTokenExpireAt) <- mkExpirationTimes
  accessToken                                 <- mkToken user jwtSettings accessTokenExpireAt
  refreshToken                                <- mkToken user jwtSettings refreshTokenExpireAt

  return $ JWT accessToken refreshToken


mkToken :: (Entity a, ToJWT a, MonadIO m) => a -> JWTSettings -> UTCTime -> m ByteString
mkToken user jwtSettings expireTime = liftIO $ makeJWT user jwtSettings (Just expireTime) >>= \case
  Right token -> return token
  Left  err   -> throwJwtException (entityId user) err


mkExpirationTimes :: (MonadTime m) => m (UTCTime, UTCTime)
mkExpirationTimes = do
  now <- currentTime
  let accessTokenExpire  = addUTCTime (secondsToNominalDiffTime 60) now
  let refreshTokenExpire = addUTCTime (nominalDay * 99) now
  return (accessTokenExpire, refreshTokenExpire)

throwJwtException :: (MonadThrow m, Show e) => UUID -> e -> m a
throwJwtException userId err =
  throwM
    $  CreateJwtException
    $  "Failed to create JWT for user with id "
    <> cs (show userId)
    <> ": "
    <> cs (show err)
