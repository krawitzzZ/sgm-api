module App.User
  ( getUsers
  , createUser
  , getUserById
  , updateUser
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

getUserById :: (C.UserRepository m) => UUID -> m User
getUserById = C.getUserById

updateUser :: (C.UserRepository m) => UUID -> Maybe Text -> Maybe Text -> m User
updateUser userId fname lname =
  C.getUserById userId >>= \u -> C.saveUser u { uFirstName = fname, uLastName = lname }

deleteUser :: (C.UserRepository m) => UUID -> m ()
deleteUser = C.deleteUser
