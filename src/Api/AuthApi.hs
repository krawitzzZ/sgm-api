module Api.AuthApi
  ( authServer
  , AuthApi
  ) where

import           Api.ApiVersion                           ( ApiVersion(..) )
import           Api.Exception                            ( ApiException(..)
                                                          , throw401
                                                          , tryCatch
                                                          , tryCatchDefault
                                                          )
import           Api.Mapper                               ( signupDtoToUserData
                                                          , userToUserDto
                                                          )
import           Api.Resources.Auth                       ( LoginDto(..)
                                                          , SignupDto
                                                          )
import           Api.Resources.User                       ( UserDto )
import           App.Auth                                 ( loginUser
                                                          , refreshJwtToken
                                                          , signupUser
                                                          )
import           Control.Exception.Safe                   ( MonadCatch
                                                          , MonadThrow
                                                          , throwM
                                                          )
import           Control.Monad.Reader.Has                 ( Has )
import           Control.Monad.Time                       ( MonadTime )
import           Data.String.Conversions                  ( cs )
import           Domain.Auth                              ( AuthenticatedUser
                                                          , JWT(..)
                                                          )
import           Domain.Class                             ( MonadLogger(..)
                                                          , UserRepository
                                                          )
import           Domain.Env                               ( Env )
import           Domain.Exception                         ( DomainException(..) )
import           Domain.Logger                            ( LogContext )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , (>>=)
                                                          , Monad
                                                          , MonadIO
                                                          , MonadReader
                                                          , Text
                                                          , return
                                                          )
import           Servant                                  ( type (:<|>)((:<|>))
                                                          , type (:>)
                                                          , Header
                                                          , Headers
                                                          , JSON
                                                          , NoContent(..)
                                                          , PostCreated
                                                          , ReqBody
                                                          , ServerT
                                                          , StdMethod(..)
                                                          , Verb
                                                          , addHeader
                                                          )
import           Servant.Auth.Server                      ( Auth
                                                          , AuthResult(..)
                                                          )
import           Servant.Exception.Server                 ( Throws )


type AuthHeaders = '[Header "X-Access-Token" Text , Header "X-Refresh-Token" Text]

type Login
  = "login" :> ReqBody '[JSON] LoginDto :> Verb 'POST 204 '[JSON] (Headers AuthHeaders NoContent)
type RefreshToken auths
  = "login" :> "refresh" :> Auth auths AuthenticatedUser :> Verb 'POST 204 '[JSON] (Headers AuthHeaders NoContent)
type Signup
  = "signup" :> ReqBody '[JSON] SignupDto :> PostCreated '[JSON] (Headers AuthHeaders UserDto)

-- brittany-disable-next-binding
type AuthApi auths = Throws ApiException :>
  (
    Login :<|>
    RefreshToken auths :<|>
    Signup
  )

type AuthApiConstraints e m
  = ( Has Env e
    , MonadReader e m
    , UserRepository m
    , MonadCatch m
    , MonadLogger m
    , MonadTime m
    , MonadIO m
    )

authServer :: (AuthApiConstraints e m) => ApiVersion -> ServerT (AuthApi auths) m
authServer V1 = authV1Server

authV1Server :: (AuthApiConstraints e m) => ServerT (AuthApi auths) m
authV1Server = login :<|> refreshToken :<|> signup


login :: (AuthApiConstraints e m) => LoginDto -> m (Headers AuthHeaders NoContent)
login LoginDto { lDtoName, lDtoPassword } = withContext (mkContext "login") $ tryCatch
  (loginUser lDtoName lDtoPassword >>= responseWithJwtHeaders NoContent)
  handleJwtException

refreshToken
  :: (AuthApiConstraints e m) => AuthResult AuthenticatedUser -> m (Headers AuthHeaders NoContent)
refreshToken (Authenticated authUser) = withContext (mkContext "refreshToken")
  $ tryCatch (refreshJwtToken authUser >>= responseWithJwtHeaders NoContent) handleJwtException
refreshToken _ = throw401

signup :: (AuthApiConstraints e m) => SignupDto -> m (Headers AuthHeaders UserDto)
signup signupDto = withContext (mkContext "signup") $ tryCatchDefault $ do
  (user, jwt) <- signupUser (signupDtoToUserData signupDto)
  responseWithJwtHeaders (userToUserDto user) jwt


responseWithJwtHeaders :: Monad m => a -> JWT -> m (Headers AuthHeaders a)
responseWithJwtHeaders response (JWT accessTok refreshTok) =
  return $ addHeader (cs accessTok) $ addHeader (cs refreshTok) response

handleJwtException :: (MonadLogger m, MonadThrow m) => DomainException -> m b
handleJwtException err@(CreateJwtException _) = do
  withError err $ logWarn "Failed to create JWT"
  throwM Unauthorized401
handleJwtException _ = throwM Unauthorized401


mkContext :: LogContext -> LogContext
mkContext = ("Api.AuthApi->" <>)
