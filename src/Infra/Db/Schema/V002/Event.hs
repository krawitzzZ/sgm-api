module Infra.Db.Schema.V002.Event
  ( EventEntity
  , EventEntityId
  , EventEntityT(..)
  , createEventsTable
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
                                                          , uuid
                                                          )
import           RIO                                      ( (.)
                                                          , Eq
                                                          , Generic
                                                          , Identity
                                                          , Show
                                                          )
import           RIO.Time                                 ( LocalTime )


data EventEntityT f = EventEntity
  { eventEntityId          :: Columnar f UUID
  , eventEntityLastUpdated :: Columnar f LocalTime
  , eventEntityCreatedAt   :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

instance Table EventEntityT where
  data PrimaryKey EventEntityT f = EventEntityId (Columnar f UUID)
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
  (EventEntity (field "id" uuid notNull)
               (field "last_updated" timestamp (defaultTo_ now_) notNull)
               (field "created_at" timestamp (defaultTo_ now_) notNull)
  )
