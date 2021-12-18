module Domain.Class
  ( MonadLogger(..)
  , UserRepository(..)
  ) where

import           Data.UUID                                ( UUID )
import           Domain.User                              ( User
                                                          , UserData
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
  getAllUsers :: m [User]
  createUser :: UserData -> m User
  saveUser :: User -> m User
  deleteUser :: UUID -> m ()
