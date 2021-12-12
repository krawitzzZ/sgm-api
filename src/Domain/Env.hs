module Domain.Env
  ( Env(..)
  , HasEnv(..)
  , setEnvLogger
  ) where

import           Domain.Config                            ( Config(..)
                                                          , HasConfig(..)
                                                          )
import           Domain.Logger                            ( Logger(..) )
import           Domain.User                              ( HasUserRepository(..)
                                                          , UserRepository(..)
                                                          )
import           RIO                                      ( id )


data Env = Env
  { envConfig         :: !Config
  , envLogger         :: !Logger
  , envUserRepository :: !UserRepository
  }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = id

instance HasConfig Env where
  getConfig = envConfig

instance HasUserRepository Env where
  getUserRepository = envUserRepository

setEnvLogger :: Logger -> Env -> Env
setEnvLogger logger env = env { envLogger = logger }
