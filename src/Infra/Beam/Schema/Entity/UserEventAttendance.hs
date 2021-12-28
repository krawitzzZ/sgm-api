module Infra.Beam.Schema.Entity.UserEventAttendance
  ( UserEventAttendancePivotT(..)
  , UserEventAttendancePivot
  , UserEventAttendancePivotId
  , PrimaryKey(UserEventAttendancePivotId)
  , mkUserEventAttendancePivot
  ) where

import           Database.Beam                                      ( Beamable
                                                                    , C
                                                                    , Table(..)
                                                                    , TableEntity
                                                                    , timestamp
                                                                    )
import           Database.Beam.Migrate                              ( CheckedDatabaseEntity
                                                                    , Migration
                                                                    , createTable
                                                                    , defaultTo_
                                                                    , field
                                                                    , notNull
                                                                    )
import           Database.Beam.Postgres                             ( Postgres
                                                                    , now_
                                                                    , uuid
                                                                    )
import           Infra.Beam.Schema.Entity.Event                     ( EventEntityT
                                                                    , PrimaryKey(..)
                                                                    )
import           Infra.Beam.Schema.Entity.User                      ( PrimaryKey(..)
                                                                    , UserEntityT
                                                                    )
import           RIO                                                ( (<$>)
                                                                    , (<*>)
                                                                    , Eq
                                                                    , Generic
                                                                    , Identity
                                                                    , Show
                                                                    )
import           RIO.Time                                           ( LocalTime )


data UserEventAttendancePivotT f = UserEventAttendancePivot
  { ueapUserId    :: !(PrimaryKey UserEntityT f)
  , ueapEventId   :: !(PrimaryKey EventEntityT f)
  , ueapCreatedAt :: !(C f LocalTime)
  }
  deriving (Generic, Beamable)

type UserEventAttendancePivot = UserEventAttendancePivotT Identity
deriving instance Show UserEventAttendancePivot
deriving instance Eq UserEventAttendancePivot

instance Table UserEventAttendancePivotT where
  data PrimaryKey UserEventAttendancePivotT f =
    UserEventAttendancePivotId !(PrimaryKey UserEntityT f) !(PrimaryKey EventEntityT f)
    deriving (Generic, Beamable)
  primaryKey = UserEventAttendancePivotId <$> ueapUserId <*> ueapEventId

type UserEventAttendancePivotId = PrimaryKey UserEventAttendancePivotT Identity
deriving instance Show UserEventAttendancePivotId
deriving instance Eq UserEventAttendancePivotId

mkUserEventAttendancePivot
  :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity UserEventAttendancePivotT))
mkUserEventAttendancePivot = createTable
  "user_event_attendance_pivot"
  (UserEventAttendancePivot (UserEntityId (field "user_id" uuid notNull))
                            (EventEntityId (field "event_id" uuid notNull))
                            (field "created_at" timestamp (defaultTo_ now_) notNull)
  )
