module Api.UserApi
  ( userServer
  , UserApi
  ) where

import           Api.ApiVersion                           ( ApiVersion(..) )
import           Api.Exception                            ( ApiException(..)
                                                          , throw401
                                                          , tryCatchDefault
                                                          )
import           Api.Mapper                               ( userToUserDto )
import           Api.Resources.User                       ( UpdateUserDto(..)
                                                          , UserDto
                                                          )
import           App.User                                 ( deleteUser
                                                          , getUserById
                                                          , getUsers
                                                          , updateUser
                                                          )
import           Control.Exception.Safe                   ( MonadCatch )
import           Data.UUID                                ( UUID
                                                          , toText
                                                          )
import           Domain.Auth                              ( AuthenticatedUser(..) )
import           Domain.Class                             ( MonadLogger(..)
                                                          , UserRepository
                                                          )
import           Domain.Logger                            ( LogContext )
import           RIO                                      ( ($)
                                                          , (<$>)
                                                          , (<>)
                                                          , (>>>)
                                                          , const
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
                                                          , Put
                                                          , ReqBody
                                                          , ServerT
                                                          )
import           Servant.Auth.Server                      ( Auth
                                                          , AuthResult(..)
                                                          )
import           Servant.Exception.Server                 ( Throws )
import           Utils                                    ( biconst )


type GetUsers = Get '[JSON] [UserDto]
type GetUser = Capture "id" UUID :> Get '[JSON] UserDto
type UpdateUser = Capture "id" UUID :> ReqBody '[JSON] UpdateUserDto :> Put '[JSON] UserDto
type DeleteUser = Capture "id" UUID :> Delete '[JSON] NoContent

-- brittany-disable-next-binding
type UserApi auths = Throws ApiException :> Auth auths AuthenticatedUser :>
  (
    GetUsers :<|>
    GetUser :<|>
    UpdateUser :<|>
    DeleteUser
  )

userServer
  :: (UserRepository m, MonadCatch m, MonadLogger m) => ApiVersion -> ServerT (UserApi auths) m
userServer V1 = userV1Server

userV1Server :: (UserRepository m, MonadCatch m, MonadLogger m) => ServerT (UserApi auths) m
userV1Server (Authenticated _) = getAllUsers :<|> findUser :<|> updateUserInfo :<|> removeUser
userV1Server _ = throw401 :<|> const throw401 :<|> biconst throw401 :<|> const throw401


getAllUsers :: (UserRepository m, MonadCatch m, MonadLogger m) => m [UserDto]
getAllUsers =
  withContext (mkContext "findUsers") $ tryCatchDefault $ map userToUserDto <$> getUsers

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
