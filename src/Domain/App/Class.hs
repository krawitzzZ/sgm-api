module Domain.App.Class
  ( MonadLogger(..)
  , UserRepository(..)
  , EventRepository(..)
  , Authentication(..)
  ) where

import           Control.Exception.Safe                             ( MonadThrow )
import           Domain.App.Types                                   ( EventId
                                                                    , UserId
                                                                    )
import           Domain.Auth                                        ( JWT )
import           Domain.Auth.Password                               ( Password
                                                                    , PasswordHash
                                                                    )
import           Domain.Auth.UserClaims                             ( UserClaims )
import           Domain.Event                                       ( Event )
import           Domain.Event.EventData                             ( NewEventData )
import           Domain.User                                        ( User )
import           Domain.User.UserData                               ( NewUserData )
import           RIO                                                ( Monad
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
  getUserById :: UserId -> m User
  getUserByUsername :: Text -> m User
  getAllUsers :: m [User]
  createUser :: NewUserData -> m User
  saveUser :: User -> m User
  deleteUser :: UserId -> m ()

class (Monad m) => EventRepository m where
  getEventById :: EventId -> m Event
  getAllEvents :: m [Event]
  createEvent :: NewEventData -> m Event
  saveEvent :: Event -> m Event
  deleteEvent :: EventId ->  m ()
  attendEvent :: Event -> EventId ->  m ()

class (Monad m) => Authentication m where
  validatePassword :: (MonadThrow m) => Password -> m ()
  checkPassword :: (MonadThrow m) => Password -> PasswordHash -> m ()
  refreshJwt :: UserClaims -> m JWT
  createJwt :: User -> m JWT
