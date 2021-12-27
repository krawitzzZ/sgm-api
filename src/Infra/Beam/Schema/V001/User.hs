module Infra.Beam.Schema.V001.User
  ( UserEntity
  , UserEntityId
  , UserEntityT(..)
  , PrimaryKey(..)
  , createUsersTable
  ) where

import           Data.UUID                                          ( UUID )
import           Database.Beam                                      ( Beamable
                                                                    , C
                                                                    , Table(..)
                                                                    , TableEntity
                                                                    , array
                                                                    , maybeType
                                                                    , timestamp
                                                                    )
import           Database.Beam.Migrate                              ( CheckedDatabaseEntity
                                                                    , Migration
                                                                    , createTable
                                                                    , defaultTo_
                                                                    , field
                                                                    , notNull
                                                                    , unique
                                                                    )
import           Database.Beam.Postgres                             ( Postgres
                                                                    , now_
                                                                    , text
                                                                    , uuid
                                                                    )
import           Domain.Auth.Password                               ( PasswordHash(..) )
import           Domain.Auth.Role                                   ( Role )
import           Infra.Beam.Schema.Types                            ( passwordType
                                                                    , roleType
                                                                    )
import           RIO                                                ( (.)
                                                                    , Eq
                                                                    , Generic
                                                                    , Identity
                                                                    , Maybe(..)
                                                                    , Show
                                                                    , Text
                                                                    , Vector
                                                                    )
import           RIO.Time                                           ( LocalTime )


data UserEntityT f = UserEntity
  { ueId            :: !(C f UUID)
  , ueCreatedAt     :: !(C f LocalTime)
  , ueLastUpdatedAt :: !(C f LocalTime)
  , ueUsername      :: !(C f Text)
  , uePassword      :: !(C f PasswordHash)
  , ueRoles         :: !(C f (Vector Role))
  , ueFirstName     :: !(C f (Maybe Text))
  , ueLastName      :: !(C f (Maybe Text))
  }
  deriving (Generic, Beamable)

type UserEntity = UserEntityT Identity
deriving instance Show UserEntity
deriving instance Eq UserEntity

instance Table UserEntityT where
  data PrimaryKey UserEntityT f = UserEntityId { userEntityId :: !(C f UUID) }
    deriving (Generic, Beamable)
  primaryKey = UserEntityId . ueId

type UserEntityId = PrimaryKey UserEntityT Identity
deriving instance Show UserEntityId
deriving instance Eq UserEntityId

createUsersTable
  :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity UserEntityT))
createUsersTable = createTable
  "users"
  (UserEntity (field "id" uuid notNull)
              (field "created_at" timestamp (defaultTo_ now_) notNull)
              (field "last_updated_at" timestamp (defaultTo_ now_) notNull)
              (field "username" text notNull unique)
              (field "password" passwordType notNull)
              (field "roles" (array roleType 9) notNull)
              (field "first_name" (maybeType text))
              (field "last_name" (maybeType text))
  )
