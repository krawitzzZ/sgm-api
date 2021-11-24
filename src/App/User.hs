module App.User
  ( getUsers
  , createUser
  , getUser
  , updateUser
  , deleteUser
  ) where

import           Domain.User                              ( HasUserRepository(..)
                                                          , Id
                                                          , User(..)
                                                          , UserRepository(..)
                                                          )
import           RIO                                      ( (.)
                                                          , (>>=)
                                                          , MonadIO
                                                          , MonadReader
                                                          , asks
                                                          , flip
                                                          , liftIO
                                                          )


getUsers :: (MonadIO m, MonadReader env m, HasUserRepository env) => m [User]
getUsers = asks getUserRepository >>= liftIO . get

createUser :: (MonadIO m, MonadReader env m, HasUserRepository env) => User -> m ()
createUser user = asks getUserRepository >>= liftIO . flip upsertOne user

getUser :: (MonadIO m, MonadReader env m, HasUserRepository env) => Id -> m User
getUser userId = asks getUserRepository >>= liftIO . flip findOne userId

updateUser :: (MonadIO m, MonadReader env m, HasUserRepository env) => Id -> User -> m ()
updateUser _ user = asks getUserRepository >>= liftIO . flip upsertOne user

deleteUser :: (MonadIO m, MonadReader env m, HasUserRepository env) => Id -> m ()
deleteUser userId = asks getUserRepository >>= liftIO . flip deleteOne userId
