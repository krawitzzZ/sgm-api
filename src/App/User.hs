module App.User
  ( getUsers
  , findUserById
  , updateUserDetails
  , deleteUser
  ) where

import           Control.Exception.Safe                             ( MonadThrow )
import           Data.UUID                                          ( UUID )
import qualified Domain.App.Class                                  as C
import           Domain.Auth.UserClaims                             ( UserClaims )
import           Domain.Policy                                      ( accessPolicyGuard )
import           Domain.User                                        ( Action(..)
                                                                    , User(..)
                                                                    )
import           RIO                                                ( (>>)
                                                                    , (>>=)
                                                                    , Maybe
                                                                    , Text
                                                                    )


getUsers :: (C.UserRepository m, MonadThrow m) => UserClaims -> m [User]
getUsers claims = accessPolicyGuard claims GetAllUsers >> C.getAllUsers

findUserById :: (C.UserRepository m, MonadThrow m) => UserClaims -> UUID -> m User
findUserById claims userId = accessPolicyGuard claims GetUser >> C.getUserById userId

updateUserDetails
  :: (C.UserRepository m, MonadThrow m) => UserClaims -> UUID -> Maybe Text -> Maybe Text -> m User
updateUserDetails claims userId uFirstName uLastName =
  accessPolicyGuard claims (UpdateUserInfo userId) >> C.getUserById userId >>= \user ->
    C.saveUser user { uFirstName, uLastName }

deleteUser :: (C.UserRepository m, MonadThrow m) => UserClaims -> UUID -> m ()
deleteUser claims userId = accessPolicyGuard claims (DeleteUser userId) >> C.deleteUser userId
