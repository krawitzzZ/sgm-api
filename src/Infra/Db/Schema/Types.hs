module Infra.Db.Schema.Types
  ( TextUUID
  , MigrationInfo
  ) where

import           Database.Beam.Migrate                    ( CheckedDatabaseSettings
                                                          , Migration
                                                          )
import           Database.Beam.Postgres                   ( Postgres )
import           RIO                                      ( Text )


type TextUUID = Text

type MigrationInfo a a'
  = ( TextUUID
    , CheckedDatabaseSettings Postgres a -> Migration Postgres (CheckedDatabaseSettings Postgres a')
    )
