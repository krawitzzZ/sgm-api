module Infra.Beam.Schema.Migration.V002
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
import           Infra.Beam.Schema.Entity.Event.V1                  ( EventEntityT
                                                                    , mkEventsTable
                                                                    )
import           Infra.Beam.Schema.Entity.User.V1                   ( UserEntityT )
import           Infra.Beam.Schema.Entity.UserEventAttendance       ( UserEventAttendancePivotT
                                                                    , mkUserEventAttendancePivot
                                                                    )
import qualified Infra.Beam.Schema.Migration.V001                  as V001
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
    <$> preserve (V001.dbUsers V001.checkedSgmDb)
    <*> mkEventsTable
    <*> mkUserEventAttendancePivot
    <*> preserve (V001.dbCryptoExtension V001.checkedSgmDb)

migrationFilename :: String
migrationFilename = "2021-12-21__V002__add_events_and_user_event_attendance_tables"
