module Infra.Beam.Schema.Migration.V003
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
                                                                    , preserve
                                                                    , runMigrationSilenced
                                                                    )
import           Database.Beam.Postgres                             ( PgExtensionEntity
                                                                    , Postgres
                                                                    )
import           Database.Beam.Postgres.PgCrypto                    ( PgCrypto )
import           Infra.Beam.MigrationUtils                          ( migrationString
                                                                    , sqlFilename
                                                                    )
import           Infra.Beam.Schema.Entity.Event.V1                  ( EventEntityT )
import           Infra.Beam.Schema.Entity.User.V2                   ( UserEntityT
                                                                    , mkUsersTable
                                                                    )
import           Infra.Beam.Schema.Entity.UserEventAttendance       ( UserEventAttendancePivotT )
import qualified Infra.Beam.Schema.Migration.V002                  as V002
import           Infra.Beam.Schema.Types                            ( MigrationMeta )
import           RIO                                                ( (<$>)
                                                                    , (<*>)
                                                                    , String
                                                                    )


data SgmDatabase f = SgmDatabase
  { dbUsers               :: f (TableEntity UserEntityT)
  , dbEvents              :: f (TableEntity EventEntityT)
  , dbUserEventAttendance :: f (TableEntity UserEventAttendancePivotT)
  , dbCryptoExtension     :: f (PgExtensionEntity PgCrypto)
  }
  deriving (Generic, (Database Postgres))

checkedSgmDb :: CheckedDatabaseSettings Postgres SgmDatabase
checkedSgmDb = runMigrationSilenced migration

migrationMeta :: MigrationMeta
migrationMeta = (sqlFilename migrationFilename, migrationString migration)

migration :: Migration Postgres (CheckedDatabaseSettings Postgres SgmDatabase)
migration =
  SgmDatabase
    <$> mkUsersTable (V002.dbUsers V002.checkedSgmDb)
    <*> preserve (V002.dbEvents V002.checkedSgmDb)
    <*> preserve (V002.dbUserEventAttendance V002.checkedSgmDb)
    <*> preserve (V002.dbCryptoExtension V002.checkedSgmDb)

migrationFilename :: String
migrationFilename = "2021-12-28__V003__add_user_profile_picture_to_users_table"
