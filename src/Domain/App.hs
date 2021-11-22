module Domain.App
  ( AppM
  , AppEnv(..)
  , HasAppEnv(..)
  ) where

import           Domain.Config                            ( Config(..)
                                                          , HasConfig(..)
                                                          )
import           Domain.Logger                            ( HasLogger(..)
                                                          , Logger(..)
                                                          )
import           Domain.User                              ( HasUserRepository(..)
                                                          , UserRepository(..)
                                                          )
import           RIO                                      ( ReaderT
                                                          , id
                                                          )
import           Servant                                  ( Handler )


-- TODO Servant dependency should be removed
type AppM = ReaderT AppEnv Handler

data AppEnv = AppEnv
  { appConfig         :: !Config
  , appLogger         :: !Logger
  , appUserRepository :: !UserRepository
  }
class HasAppEnv env where
  getAppEnv :: env -> AppEnv
instance HasAppEnv AppEnv where
  getAppEnv = id
instance HasConfig AppEnv where
  getConfig = appConfig
instance HasLogger AppEnv where
  getLogger = appLogger
instance HasUserRepository AppEnv where
  getUserRepository = appUserRepository
