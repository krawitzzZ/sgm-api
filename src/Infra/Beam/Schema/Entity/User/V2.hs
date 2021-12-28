module Infra.Beam.Schema.Entity.User.V2
  ( UserEntityT(..)
  , UserEntity
  , UserEntityId
  , PrimaryKey(UserEntityId, userEntityId)
  , mkUsersTable
  ) where

import           Database.Beam                                      ( Beamable
                                                                    , C
                                                                    , Table(..)
                                                                    , TableEntity
                                                                    , maybeType
                                                                    )
import           Database.Beam.Migrate                              ( CheckedDatabaseEntity
                                                                    , Migration
                                                                    , addColumn
                                                                    , alterTable
                                                                    , field
                                                                    )
import           Database.Beam.Postgres                             ( Postgres
                                                                    , text
                                                                    )
import           Domain.App.Types                                   ( UserId )
import           Domain.Auth.Password                               ( PasswordHash(..) )
import           Domain.Auth.Role                                   ( Role )
import qualified Infra.Beam.Schema.Entity.User.V1                  as UserV1
                                                                    ( UserEntityT(..) )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , Eq
                                                                    , Generic
                                                                    , Identity
                                                                    , Maybe(..)
                                                                    , Show
                                                                    , Text
                                                                    , Vector
                                                                    , return
                                                                    )
import           RIO.Time                                           ( LocalTime )


data UserEntityT f = UserEntity
  { ueId             :: !(C f UserId)
  , ueCreatedAt      :: !(C f LocalTime)
  , ueLastUpdatedAt  :: !(C f LocalTime)
  , ueUsername       :: !(C f Text)
  , uePassword       :: !(C f PasswordHash)
  , ueRoles          :: !(C f (Vector Role))
  , ueFirstName      :: !(C f (Maybe Text))
  , ueLastName       :: !(C f (Maybe Text))
  , ueProfilePicture :: !(C f (Maybe Text))
  }
  deriving (Generic, Beamable)

type UserEntity = UserEntityT Identity
deriving instance Show UserEntity
deriving instance Eq UserEntity

instance Table UserEntityT where
  data PrimaryKey UserEntityT f = UserEntityId { userEntityId :: !(C f UserId) }
    deriving (Generic, Beamable)
  primaryKey = UserEntityId . ueId

type UserEntityId = PrimaryKey UserEntityT Identity
deriving instance Show UserEntityId
deriving instance Eq UserEntityId

mkUsersTable
  :: CheckedDatabaseEntity Postgres db (TableEntity UserV1.UserEntityT)
  -> Migration Postgres (CheckedDatabaseEntity Postgres db' (TableEntity UserEntityT))
mkUsersTable oldTable = alterTable oldTable $ \t -> do
  profilePictureField <- addColumn (field "profile_picture" (maybeType text))
  return
    (UserEntity (UserV1.ueId t)
                (UserV1.ueCreatedAt t)
                (UserV1.ueLastUpdatedAt t)
                (UserV1.ueUsername t)
                (UserV1.uePassword t)
                (UserV1.ueRoles t)
                (UserV1.ueFirstName t)
                (UserV1.ueLastName t)
                profilePictureField
    )
