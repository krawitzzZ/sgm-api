module Infra.Beam.Schema.Latest
  ( migrations
  , sgmDb
  , V002.SgmDatabase(..)
  , module Infra.Beam.Schema.V002.User
  , module Infra.Beam.Schema.V002.Event
  , module Infra.Beam.Schema.V002.UserEventAttendance
  ) where

import           Database.Beam                                      ( DatabaseSettings )
import           Database.Beam.Migrate                              ( runMigrationSilenced
                                                                    , unCheckDatabase
                                                                    )
import           Database.Beam.Postgres                             ( Postgres )
import           Infra.Beam.Schema.Types                            ( MigrationMeta )
import qualified Infra.Beam.Schema.V001                            as V001
import qualified Infra.Beam.Schema.V002                            as V002
import           Infra.Beam.Schema.V002.Event
import           Infra.Beam.Schema.V002.User
import           Infra.Beam.Schema.V002.UserEventAttendance


sgmDb :: DatabaseSettings Postgres V002.SgmDatabase
sgmDb = unCheckDatabase (runMigrationSilenced V002.migration)

migrations :: [MigrationMeta]
migrations = [V001.migrationMeta, V002.migrationMeta]
