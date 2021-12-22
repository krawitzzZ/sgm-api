module Domain.Class
  ( MonadLogger(..)
  , UserRepository(..)
  , EventRepository(..)
  , Authentication(..)
  , Policy(..)
  ) where

import           Control.Exception.Safe                   ( MonadThrow )
import           Data.Kind                                ( Type )
import           Data.UUID                                ( UUID )
import           Domain.Auth                              ( AuthUser
                                                          , JWT
                                                          )
import           Domain.Event                             ( Event
                                                          , NewEventData
                                                          )
import           Domain.Password                          ( Password
                                                          , PasswordHash
                                                          )
import           Domain.User                              ( NewUserData
                                                          , User
                                                          )
import           RIO                                      ( Bool
                                                          , Monad
                                                          , Show
                                                          , Text
                                                          )


class MonadLogger m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logWarn :: Text -> m ()
  logError :: Text -> m ()
  withContext :: Text -> m a -> m a
  withError :: Show err => err -> m a -> m a
  withField :: (Text, Text) -> m a -> m a
  withFields :: [(Text, Text)] -> m a -> m a

class (Monad m) => UserRepository m where
  getUserById :: UUID -> m User
  getUserByUsername :: Text -> m User
  getAllUsers :: m [User]
  createUser :: NewUserData -> m User
  saveUser :: User -> m User
  deleteUser :: UUID -> m ()

class (Monad m) => EventRepository m where
  getEventById :: UUID -> m Event
  getAllEvents :: m [Event]
  createEvent :: NewEventData -> m Event
  saveEvent :: Event -> m Event
  deleteEvent :: UUID ->  m ()

class (Monad m) => Authentication m where
  validatePassword :: (MonadThrow m) => Password -> m ()
  checkPassword :: (MonadThrow m) => Password -> PasswordHash -> m ()
  createJwt :: AuthUser -> m JWT

class Policy e where
  data PolicyAction e :: Type
  policyGuard :: (MonadThrow m) => PolicyAction e -> AuthUser -> e -> m ()
  canCreate :: AuthUser -> e -> Bool
  canEdit :: AuthUser -> e -> Bool
  canDelete :: AuthUser -> e -> Bool
