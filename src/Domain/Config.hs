module Domain.Config
  ( Config(..)
  ) where

import           Control.Monad.Reader.Has                 ( Has )
import           Data.Password.Validate                   ( ValidPasswordPolicy )
import           Domain.Logger                            ( LogLevel )
import           RIO                                      ( Generic
                                                          , Int
                                                          , Text
                                                          )
import           RIO.Time                                 ( NominalDiffTime )


data Config = Config
  { cPort           :: !Int
  , cDbUrl          :: !Text
  , cNetworkTimeout :: !Int
  , cLogLevel       :: !LogLevel
  , cPasswordPolicy :: !ValidPasswordPolicy
  , cJwtDuration    :: !NominalDiffTime
  }
  deriving (Generic, Has ValidPasswordPolicy)
