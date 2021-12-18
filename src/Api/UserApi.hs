module Api.UserApi
  ( userServer
  , UserApi
  ) where

import           Api.ApiVersion                           ( ApiVersion(..) )
import           Api.Exception                            ( ApiException(..)
                                                          , tryCatchDefault
                                                          )
import           Api.Mapper                               ( createUserDtoToUserData
                                                          , userToUserDto
                                                          )
import           Api.Resources.User                       ( CreateUserDto
                                                          , UpdateUserDto(..)
                                                          , UserDto
                                                          )
import           App.User                                 ( createUser
                                                          , deleteUser
                                                          , getUserById
                                                          , getUsers
                                                          , updateUser
                                                          )
import           Control.Exception.Safe                   ( MonadCatch )
import           Data.UUID                                ( UUID
                                                          , toText
                                                          )
import           Domain.Class                             ( MonadLogger(..)
                                                          , UserRepository
                                                          )
import           Domain.Logger                            ( LogContext )
import           RIO                                      ( ($)
                                                          , (<$>)
                                                          , (<>)
                                                          , (>>>)
                                                          , map
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


type GetUsers = Get '[JSON] [UserDto]
type CreateUser = ReqBody '[JSON] CreateUserDto :> Post '[JSON] UserDto
type GetUser = Capture "id" UUID :> Get '[JSON] UserDto
type UpdateUser = Capture "id" UUID :> ReqBody '[JSON] UpdateUserDto :> Put '[JSON] UserDto
type DeleteUser = Capture "id" UUID :> Delete '[JSON] NoContent

-- brittany-disable-next-binding
type UserApi = Throws ApiException :> (GetUsers :<|> CreateUser :<|> GetUser :<|> UpdateUser :<|> DeleteUser)

userServer :: (UserRepository m, MonadCatch m, MonadLogger m) => ApiVersion -> ServerT UserApi m
userServer V1 = userV1Server

userV1Server :: (UserRepository m, MonadCatch m, MonadLogger m) => ServerT UserApi m
userV1Server = getAllUsers :<|> createNewUser :<|> findUser :<|> updateUserInfo :<|> removeUser


getAllUsers :: (UserRepository m, MonadCatch m, MonadLogger m) => m [UserDto]
getAllUsers =
  withContext (mkContext "findUsers") $ tryCatchDefault $ map userToUserDto <$> getUsers

createNewUser :: (UserRepository m, MonadCatch m, MonadLogger m) => CreateUserDto -> m UserDto
createNewUser dto = withContext (mkContext "createNewUser")
  $ tryCatchDefault (userToUserDto <$> createUser (createUserDtoToUserData dto))

findUser :: (UserRepository m, MonadCatch m, MonadLogger m) => UUID -> m UserDto
findUser userId =
  withContext (mkContext "findUser") >>> withField ("userId", toText userId) $ tryCatchDefault
    (userToUserDto <$> getUserById userId)

updateUserInfo
  :: (UserRepository m, MonadCatch m, MonadLogger m) => UUID -> UpdateUserDto -> m UserDto
updateUserInfo userId (UpdateUserDto fname lname) =
  withContext (mkContext "updateUserInfo") >>> withField ("userId", toText userId) $ tryCatchDefault
    (userToUserDto <$> updateUser userId fname lname)

removeUser :: (UserRepository m, MonadCatch m, MonadLogger m) => UUID -> m NoContent
removeUser userId =
  withContext (mkContext "removeUser") >>> withField ("userId", toText userId) $ do
    tryCatchDefault (deleteUser userId)
    return NoContent


mkContext :: LogContext -> LogContext
mkContext = ("Api.UserApi->" <>)
