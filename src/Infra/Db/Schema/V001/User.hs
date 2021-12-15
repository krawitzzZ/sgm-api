module Infra.Db.Schema.V001.User
  ( UserEntity
  , UserEntityId
  , UserEntityT(..)
  , createUsersTable
  ) where

import           Data.UUID                                ( UUID )
import           Database.Beam                            ( Beamable
                                                          , Columnar
                                                          , Table(..)
                                                          , TableEntity
                                                          , timestamp
                                                          )
import           Database.Beam.Migrate                    ( CheckedDatabaseEntity
                                                          , Migration
                                                          , createTable
                                                          , defaultTo_
                                                          , field
                                                          , notNull
                                                          )
import           Database.Beam.Postgres                   ( Postgres
                                                          , now_
                                                          , text
                                                          , uuid
                                                          )
import           RIO                                      ( (.)
                                                          , Eq
                                                          , Generic
                                                          , Identity
                                                          , Show
                                                          , Text
                                                          )
import           RIO.Time                                 ( LocalTime )


data UserEntityT f = UserEntity
  { userEntityId          :: Columnar f UUID
  , userEntityLastUpdated :: Columnar f LocalTime
  , userEntityCreatedAt   :: Columnar f LocalTime
  , userEntityName        :: Columnar f Text
  , userEntityFirstName   :: Columnar f Text
  , userEntityLastName    :: Columnar f Text
  , userEntityPassword    :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table UserEntityT where
  data PrimaryKey UserEntityT f = UserEntityId (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = UserEntityId . userEntityId

type UserEntityId = PrimaryKey UserEntityT Identity
type UserEntity = UserEntityT Identity

deriving instance Show UserEntity
deriving instance Eq UserEntity

createUsersTable
  :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity UserEntityT))
createUsersTable = createTable
  "users"
  (UserEntity (field "id" uuid notNull)
              (field "last_updated" timestamp (defaultTo_ now_) notNull)
              (field "created_at" timestamp (defaultTo_ now_) notNull)
              (field "name" text notNull)
              (field "first_name" text notNull)
              (field "last_name" text notNull)
              (field "password" text notNull)
  )
