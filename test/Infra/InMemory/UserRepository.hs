module Infra.InMemory.UserRepository
  ( InMemoryUsers(..)
  , getAllUsers
  , getUserById
  , getUserByUsername
  , createUser
  , saveUser
  , deleteUser
  ) where

import           Control.Exception.Safe                             ( MonadThrow
                                                                    , throwM
                                                                    )
import           Data.UUID                                          ( toText )
import           Data.UUID.V4                                       ( nextRandom )
import           Domain.App.Types                                   ( UserId(..) )
import           Domain.Auth.Password                               ( hashPassword )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.User                                        ( User(..) )
import           Domain.User.UserData                               ( NewUserData(..) )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (/=)
                                                                    , (<&>)
                                                                    , (<>)
                                                                    , (==)
                                                                    , Maybe(..)
                                                                    , MonadIO
                                                                    , TMVar
                                                                    , TVar
                                                                    , Text
                                                                    , atomically
                                                                    , liftIO
                                                                    , otherwise
                                                                    , readTVarIO
                                                                    , return
                                                                    , writeTVar
                                                                    )
import           RIO.List                                           ( filter
                                                                    , find
                                                                    , map
                                                                    )


data InMemoryUsers = InMemoryUsers
  { imUsers :: !(TVar [User])
  , imLock  :: !(TMVar ())
  }

getAllUsers :: (MonadIO m) => InMemoryUsers -> m [User]
getAllUsers InMemoryUsers {..} = readTVarIO imUsers

getUserById :: (MonadIO m, MonadThrow m) => InMemoryUsers -> UserId -> m User
getUserById InMemoryUsers {..} uid = do
  us <- readTVarIO imUsers
  case find (\u' -> uId u' == uid) us of
    Nothing -> throwM . NotFound $ "User with id '" <> toText (unUserId uid) <> "' not found"
    Just u  -> return u

getUserByUsername :: (MonadIO m, MonadThrow m) => InMemoryUsers -> Text -> m User
getUserByUsername InMemoryUsers {..} name = do
  us <- readTVarIO imUsers
  case find (\u' -> uUsername u' == name) us of
    Nothing -> throwM . NotFound $ "User with username '" <> name <> "' not found"
    Just u  -> return u

createUser :: (MonadIO m, MonadThrow m) => InMemoryUsers -> NewUserData -> m User
createUser InMemoryUsers {..} NewUserData {..} = do
  us <- readTVarIO imUsers
  case find (\u' -> uUsername u' == nudUsername) us of
    Just _ ->
      throwM . UserNameAlreadyExists $ "User with username '" <> nudUsername <> "' already exists"
    Nothing -> createAndReturn us
 where
  createAndReturn us = do
    uid <- liftIO $ nextRandom <&> UserId
    pwd <- hashPassword nudPassword
    let user = User uid nudUsername pwd nudRoles nudFirstName nudLastName
    atomically $ writeTVar imUsers (us <> [user])
    return user

saveUser :: (MonadIO m) => InMemoryUsers -> User -> m User
saveUser InMemoryUsers {..} user = do
  us <- readTVarIO imUsers
  let us' = map replaceWithNewUser us
  atomically $ writeTVar imUsers us'
  return user
 where
  replaceWithNewUser u | uId u == uId user = user
                       | otherwise         = u

deleteUser :: (MonadIO m) => InMemoryUsers -> UserId -> m ()
deleteUser InMemoryUsers {..} uid = do
  us <- readTVarIO imUsers
  let us' = filter (\u -> uId u /= uid) us
  atomically $ writeTVar imUsers us'
