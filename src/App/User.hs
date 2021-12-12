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


getUsers :: (MonadReader env m, HasUserRepository env, MonadIO m) => m [User]
getUsers = asks getUserRepository >>= liftIO . get

createUser :: (MonadReader env m, HasUserRepository env, MonadIO m) => User -> m ()
createUser user = asks getUserRepository >>= liftIO . flip upsertOne user

getUser :: (MonadReader env m, HasUserRepository env, MonadIO m) => Id -> m User
getUser userId = asks getUserRepository >>= liftIO . flip findOne userId

updateUser :: (MonadReader env m, HasUserRepository env, MonadIO m) => Id -> User -> m ()
updateUser _ user = asks getUserRepository >>= liftIO . flip upsertOne user

deleteUser :: (MonadReader env m, HasUserRepository env, MonadIO m) => Id -> m ()
deleteUser userId = asks getUserRepository >>= liftIO . flip deleteOne userId
