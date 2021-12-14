module Infra.Db.Schema.V002.Event
  ( EventEntity
  , EventEntityId
  , EventEntityT(..)
  , createEventsTable
  ) where

import           Database.Beam                            ( Beamable
                                                          , Columnar
                                                          , Table(..)
                                                          , TableEntity
                                                          , timestamp
                                                          )
import           Database.Beam.Backend                    ( SqlSerial )
import           Database.Beam.Migrate                    ( CheckedDatabaseEntity
                                                          , Migration
                                                          , createTable
                                                          , defaultTo_
                                                          , field
                                                          , notNull
                                                          )
import           Database.Beam.Postgres                   ( Postgres
                                                          , now_
                                                          , serial
                                                          )
import           RIO                                      ( (.)
                                                          , Eq
                                                          , Generic
                                                          , Identity
                                                          , Int32
                                                          , Show
                                                          )
import           RIO.Time                                 ( LocalTime )


data EventEntityT f = EventEntity
  { eventEntityId          :: Columnar f (SqlSerial Int32)
  , eventEntityLastUpdated :: Columnar f LocalTime
  , eventEntityCreatedAt   :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

instance Table EventEntityT where
  data PrimaryKey EventEntityT f = EventEntityId (Columnar f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = EventEntityId . eventEntityId

type EventEntityId = PrimaryKey EventEntityT Identity
type EventEntity = EventEntityT Identity

deriving instance Show EventEntity
deriving instance Eq EventEntity

createEventsTable
  :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity EventEntityT))
createEventsTable = createTable
  "events"
  (EventEntity (field "id" serial)
               (field "last_updated" timestamp (defaultTo_ now_) notNull)
               (field "created_at" timestamp (defaultTo_ now_) notNull)
  )
