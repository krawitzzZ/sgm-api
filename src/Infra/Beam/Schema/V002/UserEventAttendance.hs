module Infra.Beam.Schema.V002.UserEventAttendance
  ( UserEventAttendancePivot
  , UserEventAttendancePivotId
  , UserEventAttendancePivotT(..)
  , PrimaryKey(..)
  , createUserEventAttendanceTable
  ) where

import           Database.Beam                            ( Beamable
                                                          , C
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
                                                          , uuid
                                                          )
import           Infra.Beam.Schema.V002.Event             ( EventEntityT
                                                          , PrimaryKey(EventEntityId)
                                                          )
import           Infra.Beam.Schema.V002.User              ( PrimaryKey(UserEntityId)
                                                          , UserEntityT
                                                          )
import           RIO                                      ( (<$>)
                                                          , (<*>)
                                                          , Generic
                                                          , Identity
                                                          )
import           RIO.Time                                 ( LocalTime )


data UserEventAttendancePivotT f = UserEventAttendancePivot
  { ueapUserId    :: !(PrimaryKey UserEntityT f)
  , ueapEventId   :: !(PrimaryKey EventEntityT f)
  , ueapCreatedAt :: !(C f LocalTime)
  }
  deriving (Generic, Beamable)

type UserEventAttendancePivot = UserEventAttendancePivotT Identity

instance Table UserEventAttendancePivotT where
  data PrimaryKey UserEventAttendancePivotT f = UserEventAttendancePivotId !(PrimaryKey UserEntityT f) !(PrimaryKey EventEntityT f)
    deriving (Generic, Beamable)
  primaryKey = UserEventAttendancePivotId <$> ueapUserId <*> ueapEventId

type UserEventAttendancePivotId = PrimaryKey UserEventAttendancePivotT Identity

createUserEventAttendanceTable
  :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity UserEventAttendancePivotT))
createUserEventAttendanceTable = createTable
  "user_event_attendance_pivot"
  (UserEventAttendancePivot (UserEntityId (field "user_id" uuid notNull))
                            (EventEntityId (field "event_id" uuid notNull))
                            (field "created_at" timestamp (defaultTo_ now_) notNull)
  )
