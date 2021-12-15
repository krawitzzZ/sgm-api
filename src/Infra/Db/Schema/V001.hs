module Infra.Db.Schema.V001
  ( SgmDatabase(..)
  , migrationMeta
  ) where

import           Database.Beam                            ( Database
                                                          , Generic
                                                          , TableEntity
                                                          )
import           Database.Beam.Migrate                    ( CheckedDatabaseSettings
                                                          , Migration
                                                          )
import           Database.Beam.Postgres                   ( PgExtensionEntity
                                                          , Postgres
                                                          , pgCreateExtension
                                                          )
import           Database.Beam.Postgres.PgCrypto          ( PgCrypto )
import           Infra.Db.Schema.Types                    ( TextUUID )
import           Infra.Db.Schema.V001.User                ( UserEntityT
                                                          , createUsersTable
                                                          )
import           RIO                                      ( (<$>)
                                                          , (<*>)
                                                          )


data SgmDatabase f = SgmDatabase
  { users           :: f (TableEntity UserEntityT)
  , cryptoExtension :: f (PgExtensionEntity PgCrypto)
  }
  deriving (Generic, (Database Postgres))

-- | migrationMeta :: MigrationInfo () SgmDatabase - first migration, has to be special,
--   because there is no "from" database
migrationMeta
  :: (TextUUID, () -> Migration Postgres (CheckedDatabaseSettings Postgres SgmDatabase))
migrationMeta = (v001, migration)

v001 :: TextUUID
v001 = "00574d32-a903-49be-ae83-e309945b7075"

migration :: () -> Migration Postgres (CheckedDatabaseSettings Postgres SgmDatabase)
migration () = SgmDatabase <$> createUsersTable <*> pgCreateExtension
