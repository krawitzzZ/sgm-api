module Infra.Db.Schema.Latest
  ( migrationSteps
  , V002.SgmDatabase(..)
  , module Infra.Db.Schema.V002.User
  , module Infra.Db.Schema.V002.Event
  ) where

import           Database.Beam.Migrate                    ( CheckedDatabaseSettings
                                                          , MigrationSteps
                                                          , migrationStep
                                                          )
import           Database.Beam.Postgres                   ( Postgres )
import qualified Infra.Db.Schema.V001                    as V001
import qualified Infra.Db.Schema.V002                    as V002
import           Infra.Db.Schema.V002.Event
import           Infra.Db.Schema.V002.User
import           RIO                                      ( (>>>) )


migrationSteps :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V002.SgmDatabase)
migrationSteps =
  let (v001MigrationId, v001Migration) = V001.migrationMeta
      (v002MigrationId, v002Migration) = V002.migrationMeta
  in  migrationStep v001MigrationId v001Migration >>> migrationStep v002MigrationId v002Migration
