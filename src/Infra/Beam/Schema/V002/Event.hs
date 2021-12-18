module Infra.Beam.Schema.V002.Event
  ( EventEntity
  , EventEntityId
  , EventEntityT(..)
  , PrimaryKey(..)
  , createEventsTable
  ) where

import           Data.UUID                                ( UUID )
import           Database.Beam                            ( Beamable
                                                          , C
                                                          , Table(..)
                                                          , TableEntity
                                                          , maybeType
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
import           Infra.Beam.Schema.V002.User              ( PrimaryKey(..)
                                                          , UserEntityT
                                                          )
import           RIO                                      ( (.)
                                                          , Eq
                                                          , Generic
                                                          , Identity
                                                          , Maybe
                                                          , Show
                                                          , Text
                                                          )
import           RIO.Time                                 ( LocalTime )


data EventEntityT f = EventEntity
  { eventEntityId            :: !(C f UUID)
  , eventEntityCreatedAt     :: !(C f LocalTime)
  , eventEntityLastUpdatedAt :: !(C f LocalTime)
  , eventEntityTitle         :: !(C f Text)
  , eventEntityDescription   :: !(C f (Maybe Text))
  , eventEntityStart         :: !(C f LocalTime)
  , eventEntityEnd           :: !(C f LocalTime)
  , eventEntityCreatedBy     :: !(PrimaryKey UserEntityT f)
  , eventEntityLastUpdatedBy :: !(PrimaryKey UserEntityT f)
  }
  deriving (Generic, Beamable)

type EventEntity = EventEntityT Identity
deriving instance Show EventEntity
deriving instance Eq EventEntity

instance Table EventEntityT where
  data PrimaryKey EventEntityT f = EventEntityId !(C f UUID)
    deriving (Generic, Beamable)
  primaryKey = EventEntityId . eventEntityId

type EventEntityId = PrimaryKey EventEntityT Identity
deriving instance Show EventEntityId
deriving instance Eq EventEntityId

createEventsTable
  :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity EventEntityT))
createEventsTable = createTable
  "events"
  (EventEntity (field "id" uuid notNull)
               (field "created_at" timestamp (defaultTo_ now_) notNull)
               (field "last_updated_at" timestamp (defaultTo_ now_) notNull)
               (field "title" text notNull)
               (field "description" (maybeType text))
               (field "start" timestamp notNull)
               (field "end" timestamp notNull)
               (UserEntityId (field "created_by" uuid notNull))
               (UserEntityId (field "last_updated_by" uuid notNull))
  )
