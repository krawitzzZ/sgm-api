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
import           RIO                                      ( ($)
                                                          , MonadIO
                                                          , MonadReader
                                                          , asks
                                                          , liftIO
                                                          )


getUsers :: (MonadIO m, MonadReader env m, HasUserRepository env) => m [User]
getUsers = do
  UserRepository { get } <- asks getUserRepository
  liftIO get

createUser :: (MonadIO m, MonadReader env m, HasUserRepository env) => User -> m ()
createUser user = do
  UserRepository { upsertOne } <- asks getUserRepository
  liftIO $ upsertOne user

getUser :: (MonadIO m, MonadReader env m, HasUserRepository env) => Id -> m User
getUser userId = do
  UserRepository { findOne } <- asks getUserRepository
  liftIO $ findOne userId

updateUser :: (MonadIO m, MonadReader env m, HasUserRepository env) => Id -> User -> m ()
updateUser _ user = do
  UserRepository { upsertOne } <- asks getUserRepository
  liftIO $ upsertOne user

deleteUser :: (MonadIO m, MonadReader env m, HasUserRepository env) => Id -> m ()
deleteUser userId = do
  UserRepository { deleteOne } <- asks getUserRepository
  liftIO $ deleteOne userId
