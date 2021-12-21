module Domain.Auth
  ( AuthenticatedUser(..)
  , JWT(..)
  , mkJwt
  , mkAuthenticatedUser
  ) where

import           Control.Exception.Safe                   ( throwM )
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
import           Domain.Exception                         ( DomainException(..) )
import           Domain.User                              ( User(..) )
import           RIO                                      ( ($)
                                                          , (*)
                                                          , (<$>)
                                                          , (<*>)
                                                          , (<>)
                                                          , (>>=)
                                                          , Either(..)
                                                          , Eq
                                                          , Generic
                                                          , Maybe(..)
                                                          , MonadIO
                                                          , Text
                                                          , liftIO
                                                          , return
                                                          , show
                                                          )
import           RIO.Time                                 ( NominalDiffTime
                                                          , UTCTime
                                                          , addUTCTime
                                                          , nominalDay
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
  deriving (Has UUID, Eq, Generic)

instance ToJSON AuthenticatedUser where
  toJSON = genericToJSON $ jsonOptions "au"
instance FromJSON AuthenticatedUser where
  parseJSON = genericParseJSON $ jsonOptions "au"
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

mkAuthenticatedUser :: User -> AuthenticatedUser
mkAuthenticatedUser User { uId, uUsername } = AuthenticatedUser { auId = uId, auName = uUsername }

data JWT = JWT
  { jwtAccessToken  :: !ByteString
  , jwtRefreshToken :: !ByteString
  }
  deriving (Eq, Generic)

mkJwt :: (MonadTime m, MonadIO m) => NominalDiffTime -> JWTSettings -> AuthenticatedUser -> m JWT
mkJwt tokenDuration jwtSettings user =
  mkExpirationTimes tokenDuration >>= \(accessExpire, refreshExpire) ->
    JWT <$> mkToken user jwtSettings accessExpire <*> mkToken user jwtSettings refreshExpire


mkExpirationTimes :: (MonadTime m) => NominalDiffTime -> m (UTCTime, UTCTime)
mkExpirationTimes tokenDuration =
  currentTime >>= \now -> return (addUTCTime tokenDuration now, addUTCTime (nominalDay * 99) now)

mkToken :: (Has UUID a, ToJWT a, MonadIO m) => a -> JWTSettings -> UTCTime -> m ByteString
mkToken user jwtSettings expireTime = liftIO $ makeJWT user jwtSettings (Just expireTime) >>= \case
  Right token -> return token
  Left err ->
    throwM
      $  CreateJwtException
      $  "Failed to create JWT for user with id "
      <> cs (show (extract user :: UUID))
      <> ": "
      <> cs (show err)
