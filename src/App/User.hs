module App.User
  ( getUsers
  , findUserById
  , updateUserDetails
  , deleteUser
  ) where

import qualified Domain.App.Class                                  as C
import           Domain.App.Types                                   ( UserId )
import           Domain.Auth.UserClaims                             ( UserClaims )
import           Domain.User                                        ( Action(..)
                                                                    , User(..)
                                                                    )
import           RIO                                                ( (>>)
                                                                    , (>>=)
                                                                    , Maybe
                                                                    , Text
                                                                    )


getUsers :: (C.AccessPolicyGuard m, C.UserRepository m) => UserClaims -> m [User]
getUsers uc = C.checkPolicy uc GetAllUsers >> C.getAllUsers

findUserById :: (C.AccessPolicyGuard m, C.UserRepository m) => UserClaims -> UserId -> m User
findUserById uc uid = C.checkPolicy uc GetUser >> C.getUserById uid

updateUserDetails
  :: (C.AccessPolicyGuard m, C.UserRepository m)
  => UserClaims
  -> UserId
  -> Maybe Text
  -> Maybe Text
  -> m User
updateUserDetails uc uid uFirstName uLastName =
  C.checkPolicy uc (UpdateUserInfo uid) >> C.getUserById uid >>= \user ->
    C.saveUser user { uFirstName, uLastName }

deleteUser :: (C.AccessPolicyGuard m, C.UserRepository m) => UserClaims -> UserId -> m ()
deleteUser uc uid = C.checkPolicy uc (DeleteUser uid) >> C.deleteUser uid
