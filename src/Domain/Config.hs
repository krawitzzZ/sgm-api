module Domain.Config
  ( Config(..)
  , HasConfig(..)
  ) where

import           Domain.Logger.LogLevel                   ( LogLevel )
import           RIO                                      ( Int
                                                          , Text
                                                          , id
                                                          )


data Config = Config
  { configPort           :: !Int
  , configDbUrl          :: !Text
  , configNetworkTimeout :: !Int
  , configLogLevel       :: !LogLevel
  }
class HasConfig env where
  getConfig :: env -> Config
instance HasConfig Config where
  getConfig = id
