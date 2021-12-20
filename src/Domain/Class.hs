module Domain.Class
  ( Entity(..)
  , MonadLogger(..)
  , UserRepository(..)
  , Repository(..)
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
  getUserByName :: Text -> m User
  getAllUsers :: m [User]
  createUser :: UserData -> m User
  saveUser :: User -> m User
  deleteUser :: UUID -> m ()

class Entity entity where
  entityId :: entity -> UUID

class (Monad m, Entity e) => Repository e m where
  getById :: UUID -> m e
  -- getOneBy :: fieldAndValueOrPredicate -> m e -- HOW?
  -- getBy :: Text -> m [e]
  getAll :: m [e]
  create :: eData -> m e
  save :: e -> m e
  delete :: e -> m ()
