module App.User
  ( getUsers
  , createUser
  , findUserById
  , updateUserDetails
  , deleteUser
  ) where

import           Data.UUID                                ( UUID )
import qualified Domain.Class                            as C
import           Domain.User                              ( NewUserData
                                                          , User(..)
                                                          )
import           RIO                                      ( (>>=)
                                                          , Maybe
                                                          , Text
                                                          )


getUsers :: (C.UserRepository m) => m [User]
getUsers = C.getAllUsers

createUser :: (C.UserRepository m) => NewUserData -> m User
createUser = C.createUser

findUserById :: (C.UserRepository m) => UUID -> m User
findUserById = C.getUserById

-- TODO use Policy
updateUserDetails :: (C.UserRepository m) => UUID -> Maybe Text -> Maybe Text -> m User
updateUserDetails userId uFirstName uLastName =
  C.getUserById userId >>= \u -> C.saveUser u { uFirstName, uLastName }

-- TODO use Policy
deleteUser :: (C.UserRepository m) => UUID -> m ()
deleteUser = C.deleteUser
