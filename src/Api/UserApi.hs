module Api.UserApi
  ( userServer
  , UserApi
  ) where

import qualified Api.Exception                           as HttpException
import           Api.Helpers                              ( tryCatch
                                                          , tryCatchAny
                                                          )
import           App.User                                 ( createUser
                                                          , deleteUser
                                                          , getUser
                                                          , getUsers
                                                          , updateUser
                                                          )
import           Control.Exception.Safe                   ( MonadCatch
                                                          , MonadThrow
                                                          , throwM
                                                          )
import           Domain.Api                               ( ApiVersion(..) )
import           Domain.Exception                         ( DomainException(..) )
import           Domain.Logger.Class                      ( MonadLogger(..) )
import           Domain.Logger.LogMessage                 ( LogPath )
import           Domain.User                              ( HasUserRepository
                                                          , Id
                                                          , User(..)
                                                          )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<>)
                                                          , MonadIO
                                                          , MonadReader(..)
                                                          , return
                                                          , void
                                                          )
import           Servant                                  ( type (:<|>)(..)
                                                          , type (:>)
                                                          , Capture
                                                          , Delete
                                                          , Get
                                                          , JSON
                                                          , NoContent(..)
                                                          , Post
                                                          , Put
                                                          , ReqBody
                                                          , ServerT
                                                          )
import           Servant.Exception.Server                 ( Throws )


type GetUsers = Get '[JSON] [User]
type CreateUser = ReqBody '[JSON] User :> Post '[JSON] User
type GetUser = Capture "id" Id :> Get '[JSON] User
type UpdateUser = Capture "id" Id :> ReqBody '[JSON] User :> Put '[JSON] User
type DeleteUser = Capture "id" Id :> Delete '[JSON] NoContent

-- brittany-disable-next-binding
type UserApi = Throws HttpException.ApiException :> (GetUsers :<|> CreateUser :<|> GetUser :<|> UpdateUser :<|> DeleteUser)

userServer
  :: (HasUserRepository env, MonadReader env m, MonadCatch m, MonadIO m, MonadLogger m)
  => ApiVersion
  -> ServerT UserApi m
userServer V1 = userV1Server

userV1Server
  :: (HasUserRepository env, MonadReader env m, MonadCatch m, MonadIO m, MonadLogger m)
  => ServerT UserApi m
userV1Server = findUsers :<|> createNewUser :<|> findUser :<|> updateUserInfo :<|> removeUser

mkContext :: LogPath -> LogPath
mkContext = ("Api.UserApi->" <>)

findUsers
  :: (HasUserRepository env, MonadReader env m, MonadCatch m, MonadIO m, MonadLogger m) => m [User]
findUsers = tryCatchAny getUsers

createNewUser
  :: (HasUserRepository env, MonadReader env m, MonadCatch m, MonadIO m, MonadLogger m)
  => User
  -> m User
createNewUser userInfo = do
  void . tryCatchAny $ createUser userInfo
  return userInfo

findUser
  :: (HasUserRepository env, MonadReader env m, MonadCatch m, MonadIO m, MonadLogger m)
  => Id
  -> m User
findUser userId = withContext context . withField ("userId", userId) $ do
  logInfo "fetching user"
  tryCatch try catch
 where
  try     = getUser userId
  catch   = handleDomainException
  context = mkContext "findUser"

updateUserInfo
  :: (HasUserRepository env, MonadReader env m, MonadCatch m, MonadIO m, MonadLogger m)
  => Id
  -> User
  -> m User
updateUserInfo userId userInfo = do
  tryCatch try catch
  return userInfo
 where
  try   = updateUser userId userInfo
  catch = handleDomainException

removeUser
  :: (HasUserRepository env, MonadReader env m, MonadCatch m, MonadIO m, MonadLogger m)
  => Id
  -> m NoContent
removeUser userId = do
  tryCatch try catch
  return NoContent
 where
  try   = deleteUser userId
  catch = handleDomainException

handleDomainException :: (MonadThrow m) => (DomainException -> m a)
handleDomainException NotFound{} = throwM $ HttpException.NotFound "Not found"
handleDomainException _          = throwM HttpException.InternalError
