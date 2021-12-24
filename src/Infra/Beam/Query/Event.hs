module Infra.Beam.Query.Event
  ( allEvents
  , maybeEventById
  , createAndInsertEvent
  , updateEventDetails
  , deleteEvent
  ) where

import           Control.Monad.Reader.Has                 ( Has )
import           Data.UUID                                ( UUID )
import           Database.Beam                            ( (<-.)
                                                          , (==.)
                                                          , all_
                                                          , currentTimestamp_
                                                          , delete
                                                          , insert
                                                          , insertExpressions
                                                          , lookup_
                                                          , runDelete
                                                          , runSelectReturningList
                                                          , runSelectReturningOne
                                                          , runUpdate
                                                          , select
                                                          , update
                                                          , val_
                                                          )
import           Database.Beam.Backend.SQL.BeamExtensions ( runInsertReturningList )
import           Database.Beam.Postgres                   ( Connection )
import           Database.Beam.Postgres.PgCrypto          ( PgCrypto(..) )
import           Domain.Event                             ( Event(..) )
import           Domain.Event.EventData                   ( NewEventData(..) )
import           Infra.Beam.Query                         ( eventsTable
                                                          , pgCrypto
                                                          , runBeam
                                                          )
import           Infra.Beam.Schema.Latest                 ( EventEntity
                                                          , EventEntityT(..)
                                                          , PrimaryKey(..)
                                                          )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , Maybe
                                                          , MonadIO
                                                          , return
                                                          )


allEvents :: (Has Connection c, MonadIO m) => c -> m [EventEntity]
allEvents c = runBeam c (runSelectReturningList $ select $ all_ eventsTable)

maybeEventById :: (Has Connection c, MonadIO m) => c -> UUID -> m (Maybe EventEntity)
maybeEventById c eId = runBeam c $ runSelectReturningOne $ lookup_ eventsTable (EventEntityId eId)

createAndInsertEvent :: (Has Connection c, MonadIO m) => c -> NewEventData -> m EventEntity
createAndInsertEvent c NewEventData {..} = runBeam c $ do
  let PgCrypto {..} = pgCrypto
  [eventEntity] <- runInsertReturningList $ insert eventsTable $ insertExpressions
    [ EventEntity { eeId            = pgCryptoGenRandomUUID
                  , eeCreatedAt     = currentTimestamp_
                  , eeLastUpdatedAt = currentTimestamp_
                  , eeTitle         = val_ nedTitle
                  , eeDescription   = val_ nedDescription
                  , eeCreatedBy     = val_ (UserEntityId nedCreatedBy)
                  , eeLastUpdatedBy = val_ (UserEntityId nedCreatedBy)
                  , eeStart         = val_ nedStart
                  , eeEnd           = val_ nedEnd
                  }
    ]
  return eventEntity

updateEventDetails :: (Has Connection c, MonadIO m) => c -> Event -> m ()
updateEventDetails c Event {..} = runBeam c $ runUpdate $ update
  eventsTable
  (\EventEntity {..} ->
    (eeLastUpdatedAt <-. currentTimestamp_)
      <> (eeTitle <-. val_ eTitle)
      <> (eeDescription <-. val_ eDescription)
      <> (eeLastUpdatedBy <-. val_ (UserEntityId eLastUpdatedBy))
      <> (eeStart <-. val_ eStart)
      <> (eeEnd <-. val_ eEnd)
  )
  (\EventEntity {..} -> eeId ==. val_ eId)

deleteEvent :: (Has Connection c, MonadIO m) => c -> UUID -> m ()
deleteEvent c eId =
  runBeam c $ runDelete $ delete eventsTable (\EventEntity {..} -> eeId ==. val_ eId)
