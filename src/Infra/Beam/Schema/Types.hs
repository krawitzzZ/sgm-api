module Infra.Beam.Schema.Types
  ( MigrationInfo
  , TextUUID
  , passwordType
  , roleType
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
import           Domain.Auth.Password                     ( PasswordHash )
import           Domain.Auth.Role                         ( Role )
import           RIO                                      ( (.)
                                                          , Text
                                                          , error
                                                          , fromMaybe
                                                          )


type TextUUID = Text

type MigrationInfo a a'
  = ( TextUUID
    , CheckedDatabaseSettings Postgres a -> Migration Postgres (CheckedDatabaseSettings Postgres a')
    )

migrationId :: TextUUID -> UUID
migrationId = fromMaybe (error "Failed to parse UUID") . fromText

passwordType :: DataType Postgres PasswordHash
passwordType = DataType pgTextType

roleType :: DataType Postgres Role
roleType = DataType pgTextType
