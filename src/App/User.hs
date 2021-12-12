module App.User
  ( getUsers
  , createUser
  , getUserById
  , updateUser
  , deleteUser
  ) where

import qualified Domain.User                             as U


getUsers :: (U.UserRepository m) => m [U.User]
getUsers = U.getAllUsers

createUser :: (U.UserRepository m) => U.UserData -> m U.User
createUser = U.createUser

getUserById :: (U.UserRepository m) => U.Id -> m U.User
getUserById = U.getUserById

updateUser :: (U.UserRepository m) => U.Id -> U.UserData -> m U.User
updateUser = U.updateUser

deleteUser :: (U.UserRepository m) => U.Id -> m ()
deleteUser = U.deleteUser
