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
                                                          , getUserById
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
import           Domain.User                              ( Id
                                                          , User(..)
                                                          , UserData(..)
                                                          , UserRepository
                                                          )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , (>>>)
                                                          , return
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
type CreateUser = ReqBody '[JSON] UserData :> Post '[JSON] User
type GetUser = Capture "id" Id :> Get '[JSON] User
type UpdateUser = Capture "id" Id :> ReqBody '[JSON] UserData :> Put '[JSON] User
type DeleteUser = Capture "id" Id :> Delete '[JSON] NoContent

-- brittany-disable-next-binding
type UserApi = Throws HttpException.ApiException :> (GetUsers :<|> CreateUser :<|> GetUser :<|> UpdateUser :<|> DeleteUser)

userServer :: (UserRepository m, MonadCatch m, MonadLogger m) => ApiVersion -> ServerT UserApi m
userServer V1 = userV1Server

userV1Server :: (UserRepository m, MonadCatch m, MonadLogger m) => ServerT UserApi m
userV1Server = findUsers :<|> createNewUser :<|> findUser :<|> updateUserInfo :<|> removeUser


findUsers :: (UserRepository m, MonadCatch m, MonadLogger m) => m [User]
findUsers = withContext (mkContext "findUsers") $ tryCatchAny getUsers

createNewUser :: (UserRepository m, MonadCatch m, MonadLogger m) => UserData -> m User
createNewUser userInfo = withContext (mkContext "createNewUser") $ tryCatchAny $ do
  createUser userInfo

findUser :: (UserRepository m, MonadCatch m, MonadLogger m) => Id -> m User
findUser userId = withContext (mkContext "findUser") >>> withField ("userId", userId) $ do
  tryCatch (getUserById userId) handleDomainException

updateUserInfo :: (UserRepository m, MonadCatch m, MonadLogger m) => Id -> UserData -> m User
updateUserInfo userId userData =
  withContext (mkContext "updateUserInfo") >>> withField ("userId", userId) $ do
    tryCatch (updateUser userId userData) handleDomainException

removeUser :: (UserRepository m, MonadCatch m, MonadLogger m) => Id -> m NoContent
removeUser userId = withContext (mkContext "removeUser") $ do
  tryCatch (deleteUser userId) handleDomainException
  return NoContent

handleDomainException :: (MonadThrow m) => (DomainException -> m a)
handleDomainException NotFound{} = throwM $ HttpException.NotFound "Not found"
handleDomainException _          = throwM HttpException.InternalError

mkContext :: LogPath -> LogPath
mkContext = ("Api.UserApi->" <>)
