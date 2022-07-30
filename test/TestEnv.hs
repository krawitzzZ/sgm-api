module TestEnv
  ( TestEnv(..)
  , testDbUrl
  , testConfig
  ) where

import           Control.Monad.Reader.Has                           ( Has )
import           Data.ByteString                                    ( ByteString )
import           Data.String.Conversions                            ( cs )
import           Database.Beam.Postgres                             ( Connection )
import           Domain.App.Config                                  ( Config(..) )
import           Domain.Logger                                      ( LogLevel(..) )
import           RIO                                                ( (*)
                                                                    , Generic
                                                                    , TMVar
                                                                    )
import           RIO.Time                                           ( secondsToNominalDiffTime )


data TestEnv = TestEnv
  { teConfig :: !Config
  , teDbConn :: !Connection
  , teLock   :: !(TMVar ())
  }
  deriving (Generic, Has Config, Has Connection)

testDbUrl :: ByteString
testDbUrl = "postgresql://postgres:password@postgres:5432/sgm_api_test"

testConfig :: Config
testConfig = Config { cPort           = 9009
                    , cDbUrl          = cs testDbUrl
                    , cNetworkTimeout = 10
                    , cLogLevel       = Info
                    , cJwtDuration    = secondsToNominalDiffTime (60 * 60)
                    }
