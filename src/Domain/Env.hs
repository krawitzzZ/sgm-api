module Domain.Env
  ( Env(..)
  ) where

import           Control.Monad.Reader.Has                 ( Has )
import           Data.Password.Validate                   ( ValidPasswordPolicy )
import           Database.Beam.Postgres                   ( Connection )
import           Domain.Config                            ( Config )
import           Domain.Logger                            ( Logger )
import           RIO                                      ( Generic )
import           Servant.Auth.Server                      ( JWTSettings )


data Env = Env
  { envConfig         :: !Config
  , envLogger         :: !Logger
  , envJwtSettings    :: !JWTSettings
  , envPasswordPolicy :: !ValidPasswordPolicy
  , envDbConn         :: !Connection
  }
  deriving (Generic, Has Config, Has Logger, Has JWTSettings, Has Connection, Has
    ValidPasswordPolicy)
