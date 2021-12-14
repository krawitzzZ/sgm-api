module Infra.Db.Schema.V001
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
                                                          )
import           Database.Beam.Postgres                   ( Postgres )
import           Infra.Db.Schema.Types                    ( TextUUID )
import           Infra.Db.Schema.V001.User                ( UserEntityT
                                                          , createUsersTable
                                                          )
import           RIO                                      ( (<$>) )


newtype SgmDatabase f = SgmDatabase
  { users  :: f (TableEntity UserEntityT)
  }
  deriving Generic
  deriving anyclass (Database Postgres)

migrationSteps :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres SgmDatabase)
migrationSteps = migrationStep v001 migration

migration :: () -> Migration Postgres (CheckedDatabaseSettings Postgres SgmDatabase)
migration () = SgmDatabase <$> createUsersTable

v001 :: TextUUID
v001 = "00574d32-a903-49be-ae83-e309945b7075"
