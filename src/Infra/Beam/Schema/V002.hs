module Infra.Beam.Schema.V002
  ( SgmDatabase(..)
  , migrationMeta
  ) where

import           Database.Beam                            ( Database
                                                          , Generic
                                                          , TableEntity
                                                          )
import           Database.Beam.Migrate                    ( CheckedDatabaseSettings
                                                          , Migration
                                                          , preserve
                                                          )
import           Database.Beam.Postgres                   ( PgExtensionEntity
                                                          , Postgres
                                                          )
import           Database.Beam.Postgres.PgCrypto          ( PgCrypto )
import           Infra.Beam.Schema.Types                  ( MigrationInfo
                                                          , TextUUID
                                                          )
import qualified Infra.Beam.Schema.V001                  as V001
import           Infra.Beam.Schema.V002.Event             ( EventEntityT
                                                          , createEventsTable
                                                          )
import           Infra.Beam.Schema.V002.Password          ( )
import           Infra.Beam.Schema.V002.User              ( UserEntityT )
import           RIO                                      ( (<$>)
                                                          , (<*>)
                                                          )


data SgmDatabase f = SgmDatabase
  { dbUsers           :: f (TableEntity UserEntityT)
  , dbEvents          :: f (TableEntity EventEntityT)
  , dbCryptoExtension :: f (PgExtensionEntity PgCrypto)
  }
  deriving (Generic, (Database Postgres))

migrationMeta :: MigrationInfo V001.SgmDatabase SgmDatabase
migrationMeta = (v002, migration)

v002 :: TextUUID
v002 = "4561e859-47cb-44eb-a10c-5a583827d9a4"

migration
  :: CheckedDatabaseSettings Postgres V001.SgmDatabase
  -> Migration Postgres (CheckedDatabaseSettings Postgres SgmDatabase)
migration oldDb = SgmDatabase <$> preserve (V001.dbUsers oldDb) <*> createEventsTable <*> preserve
  (V001.dbCryptoExtension oldDb)
