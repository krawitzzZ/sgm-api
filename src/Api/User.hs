module Api.User
  ( userServer
  , UserApi
  ) where

import           Api.Helpers                              ( tryCatch
                                                          , tryCatchAny
                                                          )
import           App.User                                 ( createUser
                                                          , deleteUser
                                                          , getUser
                                                          , getUsers
                                                          , updateUser
                                                          )
import           Data.String.Conversions                  ( cs )
import           Domain.Api                               ( ApiVersion(..) )
import           Domain.App                               ( AppM )
import           Domain.Exception                         ( NotFound(..) )
import           Domain.User                              ( Id
                                                          , User(..)
                                                          )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , return
                                                          , void
                                                          )
import           Servant                                  ( type (:<|>)(..)
                                                          , type (:>)
                                                          , Capture
                                                          , DeleteNoContent
                                                          , Get
                                                          , JSON
                                                          , NoContent(..)
                                                          , Post
                                                          , Put
                                                          , ReqBody
                                                          , ServerT
                                                          , err404
                                                          , errBody
                                                          , throwError
                                                          )


type GetUsers = Get '[JSON] [User]
type CreateUser = ReqBody '[JSON] User :> Post '[JSON] User
type GetUser = Capture "id" Id :> Get '[JSON] User
type UpdateUser = Capture "id" Id :> ReqBody '[JSON] User :> Put '[JSON] User
type DeleteUser = Capture "id" Id :> DeleteNoContent

-- brittany-disable-next-binding
type UserApi = GetUsers :<|> CreateUser :<|> GetUser :<|> UpdateUser :<|> DeleteUser

userServer :: ApiVersion -> ServerT UserApi AppM
userServer V1 = userV1Server

userV1Server :: ServerT UserApi AppM
userV1Server = findUsers :<|> createNewUser :<|> findUser :<|> updateUserInfo :<|> removeUser

findUsers :: AppM [User]
findUsers = tryCatchAny "Api.User->findUsers" getUsers

createNewUser :: User -> AppM User
createNewUser userInfo = do
  void $ tryCatchAny "Api.User->createNewUser" $ createUser userInfo
  return userInfo

findUser :: Id -> AppM User
findUser userId = do
  tryCatch "Api.User->findUser" handleNotFound $ getUser userId
  where handleNotFound = throwNotFound userId

updateUserInfo :: Id -> User -> AppM User
updateUserInfo userId userInfo = do
  tryCatch "Api.User->updateUserInfo" handleNotFound $ updateUser userId userInfo
  return userInfo
  where handleNotFound = throwNotFound userId

removeUser :: Id -> AppM NoContent
removeUser userId = do
  tryCatch "Api.User->removeUser" handleNotFound $ deleteUser userId
  return NoContent
  where handleNotFound = throwNotFound userId

throwNotFound :: Id -> (NotFound -> AppM a)
throwNotFound userId = handle
 where
  handle (NotFound _) =
    throwError err404 { errBody = cs $ "User with id " <> userId <> " not found: " }
