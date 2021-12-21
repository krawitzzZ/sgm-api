module Domain.Class
  ( Entity(..)
  , MonadLogger(..)
  , UserRepository(..)
  ) where

import           Data.UUID                                ( UUID )
import           Domain.User                              ( NewUserData
                                                          , User
                                                          )
import           RIO                                      ( Monad
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

class Entity entity where
  entityId :: entity -> UUID
