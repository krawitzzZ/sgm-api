{-# OPTIONS_GHC -Wno-orphans #-}

module Infra.Beam.Schema.V001.Role
  () where

import           Database.Beam.Backend                              ( BeamBackend
                                                                    , FromBackendRow(..)
                                                                    , HasSqlValueSyntax(..)
                                                                    , IsSql92DataTypeSyntax(..)
                                                                    , autoSqlValueSyntax
                                                                    )
import           Database.Beam.Migrate                              ( BeamMigrateSqlBackend
                                                                    , HasDefaultSqlDataType(..)
                                                                    )
import           Database.PostgreSQL.Simple.FromField               ( FromField(..)
                                                                    , ResultError(..)
                                                                    , returnError
                                                                    )
import           Database.PostgreSQL.Simple.ToField                 ( ToField(..) )
import           Domain.Auth.Role                                   ( Role )
import           RIO                                                ( (.)
                                                                    , (<$>)
                                                                    , Maybe(..)
                                                                    , String
                                                                    , readMaybe
                                                                    , return
                                                                    , show
                                                                    )
import           RIO.Partial                                        ( read )


instance (BeamMigrateSqlBackend be) => HasDefaultSqlDataType be Role where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance HasSqlValueSyntax be String => (HasSqlValueSyntax be Role) where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be String) => FromBackendRow be Role where
  fromBackendRow = read <$> fromBackendRow

instance FromField Role where
  fromField f bs = do
    maybeRole <- readMaybe <$> fromField f bs
    case maybeRole of
      Nothing -> returnError ConversionFailed f "Could not 'read' value for 'Role'"
      Just x  -> return x

instance ToField Role where
  toField = toField . show
