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
import           Domain.Event                             ( Event(..)
                                                          , NewEventData(..)
                                                          )
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
                                                          , MonadReader
                                                          , return
                                                          )


-- TODO get rid of Reader
allEvents :: (Has Connection e, MonadReader e m, MonadIO m) => m [EventEntity]
allEvents = runBeam (runSelectReturningList $ select $ all_ eventsTable)

maybeEventById :: (Has Connection e, MonadReader e m, MonadIO m) => UUID -> m (Maybe EventEntity)
maybeEventById id = runBeam $ runSelectReturningOne $ lookup_ eventsTable (EventEntityId id)

createAndInsertEvent
  :: (Has Connection e, MonadReader e m, MonadIO m) => NewEventData -> m EventEntity
createAndInsertEvent NewEventData {..} = runBeam $ do
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

updateEventDetails :: (Has Connection e, MonadReader e m, MonadIO m) => Event -> m ()
updateEventDetails Event {..} = runBeam $ runUpdate $ update
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

deleteEvent :: (Has Connection e, MonadReader e m, MonadIO m) => UUID -> m ()
deleteEvent id = runBeam $ runDelete $ delete eventsTable (\EventEntity {..} -> eeId ==. val_ id)
