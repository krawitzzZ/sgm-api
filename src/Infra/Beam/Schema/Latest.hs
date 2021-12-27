module Infra.Beam.Schema.Latest
  ( migrationSteps
  , sgmDb
  , V002.SgmDatabase(..)
  , module Infra.Beam.Schema.V002.User
  , module Infra.Beam.Schema.V002.Event
  , module Infra.Beam.Schema.V002.UserEventAttendance
  ) where

import           Database.Beam                                      ( DatabaseSettings )
import           Database.Beam.Migrate                              ( CheckedDatabaseSettings
                                                                    , MigrationSteps
                                                                    , evaluateDatabase
                                                                    , migrationStep
                                                                    , unCheckDatabase
                                                                    )
import           Database.Beam.Postgres                             ( Postgres )
import qualified Infra.Beam.Schema.V001                            as V001
import qualified Infra.Beam.Schema.V002                            as V002
import           Infra.Beam.Schema.V002.Event
import           Infra.Beam.Schema.V002.User
import           Infra.Beam.Schema.V002.UserEventAttendance
import           RIO                                                ( (>>>) )


sgmDb :: DatabaseSettings Postgres V002.SgmDatabase
sgmDb = unCheckDatabase (evaluateDatabase migrationSteps)

migrationSteps :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V002.SgmDatabase)
migrationSteps =
  let (v001MigrationId, v001Migration) = V001.migrationMeta
      (v002MigrationId, v002Migration) = V002.migrationMeta
  in  migrationStep v001MigrationId v001Migration >>> migrationStep v002MigrationId v002Migration
