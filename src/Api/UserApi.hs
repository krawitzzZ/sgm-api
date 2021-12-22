module Api.UserApi
  ( userServer
  , UserApi
  ) where

import           Api.ApiVersion                           ( ApiVersion(..) )
import           Api.Exception                            ( ApiException(..)
                                                          , throw401
                                                          , throw403
                                                          , tryCatchDefault
                                                          )
import           Api.Mapper                               ( userToUserDto )
import           Api.Resources.User                       ( UpdateUserDto(..)
                                                          , UserDto
                                                          )
import           App.User                                 ( deleteUser
                                                          , findUserById
                                                          , getUsers
                                                          , updateUserDetails
                                                          )
import           Control.Exception.Safe                   ( MonadCatch
                                                          , MonadThrow
                                                          )
import           Data.UUID                                ( UUID
                                                          , toText
                                                          )
import           Domain.Auth                              ( AuthUser(..) )
import           Domain.Class                             ( MonadLogger(..)
                                                          , UserRepository
                                                          )
import           Domain.Logger                            ( LogContext
                                                          , userIdKey
                                                          )
import           RIO                                      ( ($)
                                                          , (<$>)
                                                          , (<>)
                                                          , (==)
                                                          , (>>>)
                                                          , Text
                                                          , const
                                                          , map
                                                          , return
                                                          , unless
                                                          )
import           Servant                                  ( type (:<|>)(..)
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
import           Servant.Auth.Server                      ( Auth
                                                          , AuthResult(..)
                                                          )
import           Servant.Exception.Server                 ( Throws )
import           Utils                                    ( biconst )


type GetUsers = Get '[JSON] [UserDto]
type GetUser = Capture "id" UUID :> Get '[JSON] UserDto
type UpdateUser = Capture "id" UUID :> ReqBody '[JSON] UpdateUserDto :> Put '[JSON] UserDto
type DeleteUser = Capture "id" UUID :> Verb 'DELETE 204 '[JSON] NoContent

-- brittany-disable-next-binding
type UserApi auths = Throws ApiException :> Auth auths AuthUser :>
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
userV1Server (Authenticated u) = allUsers u :<|> userById u :<|> updateUserInfo u :<|> removeUser u
userV1Server _ = throw401 :<|> const throw401 :<|> biconst throw401 :<|> const throw401


allUsers :: (UserRepository m, MonadCatch m, MonadLogger m) => AuthUser -> m [UserDto]
allUsers me =
  withContext (mkContext "allUsers")
    >>> withField (userIdKey, toText (auId me))
    $   tryCatchDefault
    $   map userToUserDto
    <$> getUsers

userById :: (UserRepository m, MonadCatch m, MonadLogger m) => AuthUser -> UUID -> m UserDto
userById me userId =
  withContext (mkContext "userById") >>> withFields (logFields me userId) $ tryCatchDefault
    (userToUserDto <$> findUserById userId)

updateUserInfo
  :: (UserRepository m, MonadCatch m, MonadLogger m)
  => AuthUser
  -> UUID
  -> UpdateUserDto
  -> m UserDto
updateUserInfo me userId UpdateUserDto { uuDtoFirstName, uuDtoLastName } =
  withContext (mkContext "updateUserInfo") >>> withFields (logFields me userId) $ do
    identityGuard userId me
    tryCatchDefault (userToUserDto <$> updateUserDetails userId uuDtoFirstName uuDtoLastName)

removeUser :: (UserRepository m, MonadCatch m, MonadLogger m) => AuthUser -> UUID -> m NoContent
removeUser me userId =
  withContext (mkContext "removeUser") >>> withFields (logFields me userId) $ do
    identityGuard userId me -- TODO use Policy in App.User
    tryCatchDefault (deleteUser userId)
    return NoContent


identityGuard :: (MonadThrow m) => UUID -> AuthUser -> m ()
identityGuard id u = unless (auId u == id) throw403

logFields :: AuthUser -> UUID -> [(Text, Text)]
logFields me userId = [(userIdKey, toText (auId me)), ("requestedUserId", toText userId)]

mkContext :: LogContext -> LogContext
mkContext = ("Api.UserApi->" <>)
