module Domain.Env
  ( Env(..)
  ) where

import           Control.Monad.Reader.Has                 ( Has )
import           Database.Beam.Postgres                   ( Connection )
import           Domain.Config                            ( Config )
import           Domain.Logger                            ( Logger )
import           RIO                                      ( Generic )


data Env = Env
  { envConfig :: !Config
  , envLogger :: !Logger
  , envDbConn :: !Connection
  }
  deriving (Generic, Has Config, Has Logger, Has Connection)
