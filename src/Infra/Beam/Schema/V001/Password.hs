{-# OPTIONS_GHC -Wno-orphans #-}

module Infra.Beam.Schema.V001.Password where

import qualified Data.Password.Argon2                    as P
import           Database.Beam.Backend                    ( BeamBackend
                                                          , FromBackendRow(..)
                                                          , HasSqlValueSyntax(..)
                                                          , IsSql92DataTypeSyntax(..)
                                                          )
import           Database.Beam.Migrate                    ( BeamMigrateSqlBackend
                                                          , HasDefaultSqlDataType(..)
                                                          )
import           Domain.Password                          ( PasswordHash(..) )
import           RIO                                      ( ($)
                                                          , (<$>)
                                                          , Maybe(..)
                                                          , Text
                                                          )


type ArgonPasswordHash = P.PasswordHash P.Argon2

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be ArgonPasswordHash where
  fromBackendRow = P.PasswordHash <$> fromBackendRow

instance (BeamMigrateSqlBackend be) => HasDefaultSqlDataType be PasswordHash where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance HasSqlValueSyntax be Text => (HasSqlValueSyntax be PasswordHash) where
  sqlValueSyntax (PasswordHash argonPwd) = sqlValueSyntax $ P.unPasswordHash argonPwd

instance (BeamBackend be, FromBackendRow be ArgonPasswordHash) => FromBackendRow be PasswordHash where
  fromBackendRow = PasswordHash <$> fromBackendRow
