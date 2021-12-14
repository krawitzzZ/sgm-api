module Domain.Env
  ( Env(..)
  , HasEnv(..)
  , setEnvLogger
  ) where

import           Database.Beam                            ( DatabaseSettings )
import           Database.Beam.Postgres                   ( Postgres )
import           Domain.Config                            ( Config(..)
                                                          , HasConfig(..)
                                                          )
import           Domain.Logger                            ( Logger(..) )
import           Infra.Db.Schema                          ( SgmDatabase )
import           RIO                                      ( id )


data Env = Env
  { envConfig :: !Config
  , envLogger :: !Logger
  , envDb     :: !(DatabaseSettings Postgres SgmDatabase)
  }

class HasEnv env where
  getEnv :: env -> Env

instance HasEnv Env where
  getEnv = id

instance HasConfig Env where
  getConfig = envConfig

setEnvLogger :: Logger -> Env -> Env
setEnvLogger logger env = env { envLogger = logger }
