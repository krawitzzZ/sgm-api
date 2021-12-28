module Infra.Beam.Schema.V002
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
import           Infra.Beam.Schema.Types                            ( MigrationMeta )
import qualified Infra.Beam.Schema.V001                            as V001
import           Infra.Beam.Schema.V002.Event                       ( EventEntityT
                                                                    , createEventsTable
                                                                    )
import           Infra.Beam.Schema.V002.Password                    ( )
import           Infra.Beam.Schema.V002.Role                        ( )
import           Infra.Beam.Schema.V002.User                        ( UserEntityT )
import           Infra.Beam.Schema.V002.UserEventAttendance         ( UserEventAttendancePivotT
                                                                    , createUserEventAttendanceTable
                                                                    )
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
    <*> createEventsTable
    <*> createUserEventAttendanceTable
    <*> preserve (V001.dbCryptoExtension V001.checkedSgmDb)

migrationFilename :: String
migrationFilename = "2021-12-21__V002__add_events_and_user_event_attendance_tables"
