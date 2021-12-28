module Infra.Beam.Schema.Types
  ( MigrationMeta
  , passwordType
  , roleType
  ) where

import           Database.Beam                                      ( DataType(..) )
import           Database.Beam.Postgres                             ( Postgres )
import           Database.Beam.Postgres.Syntax                      ( pgTextType )
import           Domain.Auth.Password                               ( PasswordHash )
import           Domain.Auth.Role                                   ( Role )
import           RIO                                                ( FilePath
                                                                    , String
                                                                    )


type MigrationMeta = (FilePath, String)

passwordType :: DataType Postgres PasswordHash
passwordType = DataType pgTextType

roleType :: DataType Postgres Role
roleType = DataType pgTextType
