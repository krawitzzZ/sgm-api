module Infra.Beam.Schema.Migration.V001
  ( SgmDatabase(..)
  , checkedSgmDb
  , migrationMeta
  , migration
  ) where

import           Database.Beam                                      ( Database
                                                                    , Generic
                                                                    , TableEntity
                                                                    )
import           Database.Beam.Migrate                              ( CheckedDatabaseSettings
                                                                    , Migration
                                                                    , runMigrationSilenced
                                                                    )
import           Database.Beam.Postgres                             ( PgExtensionEntity
                                                                    , Postgres
                                                                    , pgCreateExtension
                                                                    )
import           Database.Beam.Postgres.PgCrypto                    ( PgCrypto )
import           Infra.Beam.MigrationUtils                          ( migrationString
                                                                    , sqlFilename
                                                                    )
import           Infra.Beam.Schema.Entity.User.V1                   ( UserEntityT
                                                                    , mkUsersTable
                                                                    )
import           Infra.Beam.Schema.Types                            ( MigrationMeta )
import           RIO                                                ( (<$>)
                                                                    , (<*>)
                                                                    , String
                                                                    )


data SgmDatabase f = SgmDatabase
  { dbUsers           :: f (TableEntity UserEntityT)
  , dbCryptoExtension :: f (PgExtensionEntity PgCrypto)
  }
  deriving (Generic, (Database Postgres))

checkedSgmDb :: CheckedDatabaseSettings Postgres SgmDatabase
checkedSgmDb = runMigrationSilenced migration

migrationMeta :: MigrationMeta
migrationMeta = (sqlFilename migrationFilename, migrationString migration)

migration :: Migration Postgres (CheckedDatabaseSettings Postgres SgmDatabase)
migration = SgmDatabase <$> mkUsersTable <*> pgCreateExtension

migrationFilename :: String
migrationFilename = "2021-12-10__V001__initial_migration_add_users_table"
