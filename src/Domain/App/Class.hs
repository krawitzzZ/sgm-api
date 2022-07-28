module Domain.App.Class
  ( MonadLogger(..)
  , Authentication(..)
  , AccessPolicyGuard(..)
  , UserRepository(..)
  , EventRepository(..)
  ) where

import           Control.Exception.Safe                             ( MonadThrow
                                                                    , throwM
                                                                    )
import           Domain.App.Types                                   ( EventId
                                                                    , UserId
                                                                    )
import           Domain.Auth                                        ( JWT )
import           Domain.Auth.Password                               ( Password
                                                                    , PasswordHash
                                                                    )
import           Domain.Auth.Permission                             ( isPermitted )
import           Domain.Auth.UserClaims                             ( UserClaims )
import           Domain.Event                                       ( Event )
import           Domain.Event.EventData                             ( NewEventData )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.Policy                                      ( HasActionPolicy(..) )
import           Domain.User                                        ( User )
import           Domain.User.UserData                               ( NewUserData )
import           RIO                                                ( ($)
                                                                    , Monad
                                                                    , Show
                                                                    , Text
                                                                    , Typeable
                                                                    , unless
                                                                    )


class (Monad m) => MonadLogger m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logWarn :: Text -> m ()
  logError :: Text -> m ()
  withContext :: Text -> m a -> m a
  withError :: Show err => err -> m a -> m a
  withField :: (Text, Text) -> m a -> m a
  withFields :: [(Text, Text)] -> m a -> m a

class (Monad m) => Authentication m where
  validatePassword :: Password -> m ()
  checkPassword :: Password -> PasswordHash -> m ()
  refreshJwt :: UserClaims -> m JWT
  createJwt :: User -> m JWT

class (MonadThrow m) => AccessPolicyGuard m where
  checkPolicy :: (Typeable e, HasActionPolicy e) => UserClaims -> Action e -> m ()
  checkPolicy uc a = unless (isPermitted $ actionPermission uc a) $ throwM AccessPolicyViolation

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
  deleteEvent :: EventId -> m ()
  attendEvent :: Event -> UserId -> m ()
  unattendEvent :: Event -> UserId -> m ()
