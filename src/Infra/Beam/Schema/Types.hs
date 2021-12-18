module Infra.Beam.Schema.Types
  ( MigrationInfo
  , TextUUID
  , passwordType
  , migrationId
  ) where

import           Data.UUID                                ( UUID
                                                          , fromText
                                                          )
import           Database.Beam                            ( DataType(..) )
import           Database.Beam.Migrate                    ( CheckedDatabaseSettings
                                                          , Migration
                                                          )
import           Database.Beam.Postgres                   ( Postgres )
import           Database.Beam.Postgres.Syntax            ( pgTextType )
import           Domain.Password                          ( PasswordHash(..) )
import           RIO                                      ( (.)
                                                          , Text
                                                          , error
                                                          , fromMaybe
                                                          )


type TextUUID = Text

migrationId :: TextUUID -> UUID
migrationId = fromMaybe (error "Failed to parse UUID") . fromText

type MigrationInfo a a'
  = ( TextUUID
    , CheckedDatabaseSettings Postgres a -> Migration Postgres (CheckedDatabaseSettings Postgres a')
    )

passwordType :: DataType Postgres PasswordHash
passwordType = DataType pgTextType
