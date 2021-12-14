module Infra.Db.Schema.V002
  ( SgmDatabase(..)
  , migrationSteps
  ) where

import           Database.Beam                            ( Database
                                                          , Generic
                                                          , TableEntity
                                                          )
import           Database.Beam.Migrate                    ( CheckedDatabaseSettings
                                                          , Migration
                                                          , MigrationSteps
                                                          , migrationStep
                                                          , preserve
                                                          )
import           Database.Beam.Postgres                   ( Postgres )
import           Infra.Db.Schema.Types                    ( TextUUID )
import qualified Infra.Db.Schema.V001                    as V001
import           Infra.Db.Schema.V002.Event               ( EventEntityT
                                                          , createEventsTable
                                                          )
import           Infra.Db.Schema.V002.User                ( UserEntityT )
import           RIO                                      ( (<$>)
                                                          , (<*>)
                                                          , (>>>)
                                                          )


data SgmDatabase f = SgmDatabase
  { users  :: f (TableEntity UserEntityT)
  , events :: f (TableEntity EventEntityT)
  }
  deriving (Generic, (Database Postgres))

migrationSteps :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres SgmDatabase)
migrationSteps = V001.migrationSteps >>> migrationStep v002 migration

migration
  :: CheckedDatabaseSettings Postgres V001.SgmDatabase
  -> Migration Postgres (CheckedDatabaseSettings Postgres SgmDatabase)
migration oldDb = SgmDatabase <$> preserve (V001.users oldDb) <*> createEventsTable

v002 :: TextUUID
v002 = "4561e859-47cb-44eb-a10c-5a583827d9a4"
