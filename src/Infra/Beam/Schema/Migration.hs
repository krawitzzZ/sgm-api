module Infra.Beam.Schema.Migration
  ( V003.SgmDatabase(..)
  , sgmDb
  , migrations
  ) where

import           Database.Beam                                      ( DatabaseSettings )
import           Database.Beam.Migrate                              ( runMigrationSilenced
                                                                    , unCheckDatabase
                                                                    )
import           Database.Beam.Postgres                             ( Postgres )
import qualified Infra.Beam.Schema.Migration.V001                  as V001
import qualified Infra.Beam.Schema.Migration.V002                  as V002
import qualified Infra.Beam.Schema.Migration.V003                  as V003
import           Infra.Beam.Schema.Types                            ( MigrationMeta )


sgmDb :: DatabaseSettings Postgres V003.SgmDatabase
sgmDb = unCheckDatabase (runMigrationSilenced V003.migration)

migrations :: [MigrationMeta]
migrations = [V001.migrationMeta, V002.migrationMeta, V003.migrationMeta]
