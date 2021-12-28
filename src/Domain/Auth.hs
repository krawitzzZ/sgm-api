module Domain.Auth
  ( JWT(..)
  , createJwt
  , refreshJwt
  ) where

import           Control.Exception.Safe                             ( throwM )
import           Control.Monad.Reader.Has                           ( Has
                                                                    , extract
                                                                    )
import           Control.Monad.Time                                 ( MonadTime(..) )
import           Data.ByteString.Lazy                               ( ByteString )
import           Data.String.Conversions                            ( cs )
import           Domain.App.Types                                   ( UserId )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.User                                        ( User(..) )
import           RIO                                                ( ($)
                                                                    , (*)
                                                                    , (<$>)
                                                                    , (<*>)
                                                                    , (<>)
                                                                    , (>>=)
                                                                    , Either(..)
                                                                    , Maybe(..)
                                                                    , MonadIO
                                                                    , liftIO
                                                                    , return
                                                                    , show
                                                                    )
import           RIO.Time                                           ( NominalDiffTime
                                                                    , UTCTime
                                                                    , addUTCTime
                                                                    , nominalDay
                                                                    )
import           Servant.Auth.JWT                                   ( ToJWT )
import           Servant.Auth.Server                                ( JWTSettings
                                                                    , makeJWT
                                                                    )


data JWT = JWT
  { jwtAccessToken  :: !ByteString
  , jwtRefreshToken :: !ByteString
  }

createJwt :: (MonadTime m, MonadIO m) => NominalDiffTime -> JWTSettings -> User -> m JWT
createJwt tokenDuration jwtSettings User {..} =
  mkExpirationTimes tokenDuration >>= mkJwt jwtSettings UserClaims { ucId = uId, ucRoles = uRoles }

refreshJwt :: (MonadTime m, MonadIO m) => NominalDiffTime -> JWTSettings -> UserClaims -> m JWT
refreshJwt tokenDuration jwtSettings user =
  mkExpirationTimes tokenDuration >>= mkJwt jwtSettings user


mkExpirationTimes :: (MonadTime m) => NominalDiffTime -> m (UTCTime, UTCTime)
mkExpirationTimes tokenDuration = currentTime >>= \now -> do
  return (addUTCTime tokenDuration now, addUTCTime (nominalDay * 99) now)

mkJwt :: (Has UserId c, ToJWT c, MonadIO m) => JWTSettings -> c -> (UTCTime, UTCTime) -> m JWT
mkJwt jwtSettings user (accessExpire, refreshExpire) =
  JWT <$> mkToken user jwtSettings accessExpire <*> mkToken user jwtSettings refreshExpire

mkToken :: (Has UserId c, ToJWT c, MonadIO m) => c -> JWTSettings -> UTCTime -> m ByteString
mkToken user jwtSettings expireTime = liftIO $ makeJWT user jwtSettings (Just expireTime) >>= \case
  Right token -> return token
  Left err ->
    throwM
      $  CreateJwtException
      $  "Failed to create JWT for user with id "
      <> cs (show (extract user :: UserId))
      <> ": "
      <> cs (show err)
