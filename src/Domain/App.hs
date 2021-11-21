module Domain.App
  ( AppM
  , Config(..)
  , HasConfig(..)
  , AppEnv(..)
  , HasAppEnv(..)
  ) where

import           Domain.Logger                            ( HasLogger(..)
                                                          , LogLevel(..)
                                                          , Logger(..)
                                                          )
import           Domain.User                              ( HasUserRepository(..)
                                                          , UserRepository(..)
                                                          )
import           RIO                                      ( Int
                                                          , ReaderT
                                                          , Text
                                                          , id
                                                          )
import           Servant                                  ( Handler )


type AppM = ReaderT AppEnv Handler

data Config = Config
  { configPort           :: !Int
  , configNetworkTimeout :: !Int
  , configVersion        :: !Text
  , configLogLevel       :: !LogLevel
  }
class HasConfig env where
  getConfig :: env -> Config
instance HasConfig Config where
  getConfig = id

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
