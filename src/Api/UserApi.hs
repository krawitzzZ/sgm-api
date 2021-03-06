module Api.UserApi
  ( userServer
  , UserApi
  ) where

import           Api.ApiVersion                                     ( ApiVersion(..) )
import           Api.Exception                                      ( ApiException(..)
                                                                    , throw401
                                                                    , tryCatchDefault
                                                                    )
import           Api.Mapper                                         ( userToUserDto )
import           Api.Resources.User                                 ( UpdateUserDto(..)
                                                                    , UserDto
                                                                    )
import           App.User                                           ( deleteUser
                                                                    , findUserById
                                                                    , getUsers
                                                                    , updateUserDetails
                                                                    )
import           Control.Exception.Safe                             ( MonadCatch )
import           Data.UUID                                          ( toText )
import           Domain.App.Class                                   ( AccessPolicyGuard
                                                                    , MonadLogger(..)
                                                                    , UserRepository
                                                                    )
import           Domain.App.Types                                   ( UserId(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Logger                                      ( LogContext
                                                                    , userIdKey
                                                                    )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<$>)
                                                                    , (<>)
                                                                    , (>>)
                                                                    , (>>>)
                                                                    , Text
                                                                    , const
                                                                    , map
                                                                    , return
                                                                    )
import           Servant                                            ( type (:<|>)(..)
                                                                    , type (:>)
                                                                    , Capture
                                                                    , Get
                                                                    , JSON
                                                                    , NoContent(..)
                                                                    , Put
                                                                    , ReqBody
                                                                    , ServerT
                                                                    , StdMethod(..)
                                                                    , Verb
                                                                    )
import           Servant.Auth.Server                                ( Auth
                                                                    , AuthResult(..)
                                                                    )
import           Servant.Exception.Server                           ( Throws )
import           Utils                                              ( biconst )


type GetUsers = Get '[JSON] [UserDto]
type GetUser = Capture "id" UserId :> Get '[JSON] UserDto
type UpdateUser = Capture "id" UserId :> ReqBody '[JSON] UpdateUserDto :> Put '[JSON] UserDto
type DeleteUser = Capture "id" UserId :> Verb 'DELETE 204 '[JSON] NoContent

-- brittany-disable-next-binding
type UserApi auths = Throws ApiException :> Auth auths UserClaims :>
  (
    GetUsers :<|>
    GetUser :<|>
    UpdateUser :<|>
    DeleteUser
  )

userServer
  :: (AccessPolicyGuard m, UserRepository m, MonadCatch m, MonadLogger m)
  => ApiVersion
  -> ServerT (UserApi auths) m
userServer V1 = userV1Server

userV1Server
  :: (AccessPolicyGuard m, UserRepository m, MonadCatch m, MonadLogger m)
  => ServerT (UserApi auths) m
userV1Server (Authenticated u) = allUsers u :<|> userById u :<|> updateUserInfo u :<|> removeUser u
userV1Server _ = throw401 :<|> const throw401 :<|> biconst throw401 :<|> const throw401


allUsers
  :: (AccessPolicyGuard m, UserRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> m [UserDto]
allUsers me =
  withContext (mkContext "allUsers")
    >>> withField (userIdKey, toText . unUserId . ucId $ me)
    $   tryCatchDefault
    $   map userToUserDto
    <$> getUsers me

userById
  :: (AccessPolicyGuard m, UserRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> UserId
  -> m UserDto
userById me userId =
  withContext (mkContext "userById") >>> withFields (logFields me userId) $ tryCatchDefault
    (userToUserDto <$> findUserById me userId)

updateUserInfo
  :: (AccessPolicyGuard m, UserRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> UserId
  -> UpdateUserDto
  -> m UserDto
updateUserInfo me userId UpdateUserDto {..} =
  withContext (mkContext "updateUserInfo") >>> withFields (logFields me userId) $ tryCatchDefault
    (userToUserDto <$> updateUserDetails me userId uuDtoFirstName uuDtoLastName)

removeUser
  :: (AccessPolicyGuard m, UserRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> UserId
  -> m NoContent
removeUser me userId =
  withContext (mkContext "removeUser")
    >>> withFields (logFields me userId)
    $   tryCatchDefault (deleteUser me userId)
    >>  return NoContent


logFields :: UserClaims -> UserId -> [(Text, Text)]
logFields me (UserId userId) =
  [(userIdKey, toText . unUserId . ucId $ me), ("requestUserId", toText userId)]

mkContext :: LogContext -> LogContext
mkContext = ("Api.UserApi->" <>)
