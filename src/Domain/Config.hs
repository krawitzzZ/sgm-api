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


data Config = Config
  { configPort           :: !Int
  , configDbUrl          :: !Text
  , configNetworkTimeout :: !Int
  , configLogLevel       :: !LogLevel
  , configPasswordPolicy :: !ValidPasswordPolicy
  }
  deriving (Generic, Has ValidPasswordPolicy)
