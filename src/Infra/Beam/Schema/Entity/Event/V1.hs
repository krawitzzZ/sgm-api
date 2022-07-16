module Infra.Beam.Schema.Entity.Event.V1
  ( EventEntityT(..)
  , EventEntity
  , EventEntityId
  , PrimaryKey(EventEntityId, eventEntityId)
  , mkEventsTable
  ) where

import           Data.UUID                                          ( UUID )
import           Database.Beam                                      ( Beamable
                                                                    , C
                                                                    , Table(..)
                                                                    , TableEntity
                                                                    , maybeType
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
                                                                    , text
                                                                    , uuid
                                                                    )
import           Infra.Beam.Schema.Entity.User                      ( PrimaryKey(..)
                                                                    , UserEntityT
                                                                    )
import           RIO                                                ( (.)
                                                                    , Eq
                                                                    , Generic
                                                                    , Identity
                                                                    , Maybe
                                                                    , Show
                                                                    , Text
                                                                    )
import           RIO.Time                                           ( LocalTime )


data EventEntityT f = EventEntity
  { eeId            :: !(C f UUID)
  , eeCreatedAt     :: !(C f LocalTime)
  , eeLastUpdatedAt :: !(C f LocalTime)
  , eeTitle         :: !(C f Text)
  , eeDescription   :: !(C f (Maybe Text))
  , eeCreatedBy     :: !(PrimaryKey UserEntityT f)
  , eeLastUpdatedBy :: !(PrimaryKey UserEntityT f)
  , eeStart         :: !(C f LocalTime)
  , eeEnd           :: !(C f LocalTime)
  }
  deriving (Generic, Beamable)

type EventEntity = EventEntityT Identity
deriving instance Show EventEntity
deriving instance Eq EventEntity

instance Table EventEntityT where
  data PrimaryKey EventEntityT f = EventEntityId { eventEntityId :: !(C f UUID) }
    deriving (Generic, Beamable)
  primaryKey = EventEntityId . eeId

type EventEntityId = PrimaryKey EventEntityT Identity
deriving instance Show EventEntityId
deriving instance Eq EventEntityId

mkEventsTable :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity EventEntityT))
mkEventsTable = createTable
  "events"
  (EventEntity (field "event_id" uuid notNull)
               (field "created_at" timestamp (defaultTo_ now_) notNull)
               (field "last_updated_at" timestamp (defaultTo_ now_) notNull)
               (field "title" text notNull)
               (field "description" (maybeType text))
               (UserEntityId (field "created_by" uuid notNull))
               (UserEntityId (field "last_updated_by" uuid notNull))
               (field "start" timestamp notNull)
               (field "end" timestamp notNull)
  )
