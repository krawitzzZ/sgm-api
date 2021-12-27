module Domain.App.Config
  ( Config(..)
  ) where

import           Domain.Logger                                      ( LogLevel )
import           RIO                                                ( Int
                                                                    , Text
                                                                    )
import           RIO.Time                                           ( NominalDiffTime )


data Config = Config
  { cPort           :: !Int
  , cDbUrl          :: !Text
  , cNetworkTimeout :: !Int
  , cLogLevel       :: !LogLevel
  , cJwtDuration    :: !NominalDiffTime
  }
