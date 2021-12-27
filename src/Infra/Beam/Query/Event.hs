module Infra.Beam.Query.Event
  ( allEvents
  , maybeEventById
  , createAndInsertEvent
  , updateEventDetails
  , deleteEvent
  , attendAtEvent
  ) where

import           Control.Monad.Reader.Has                           ( Has )
import           Data.UUID                                          ( UUID )
import           Database.Beam                                      ( (<-.)
                                                                    , (==.)
                                                                    , all_
                                                                    , currentTimestamp_
                                                                    , delete
                                                                    , filter_
                                                                    , insert
                                                                    , insertExpressions
                                                                    , leftJoin_
                                                                    , pk
                                                                    , references_
                                                                    , runDelete
                                                                    , runInsert
                                                                    , runSelectReturningList
                                                                    , runUpdate
                                                                    , select
                                                                    , update
                                                                    , val_
                                                                    )
import           Database.Beam.Backend.SQL.BeamExtensions           ( runInsertReturningList )
import           Database.Beam.Postgres                             ( Connection )
import           Database.Beam.Postgres.PgCrypto                    ( PgCrypto(..) )
import           Domain.App.Config                                  ( Config )
import           Domain.Event                                       ( Event(..) )
import           Domain.Event.EventData                             ( NewEventData(..) )
import           Infra.Beam.Query                                   ( eventsTable
                                                                    , pgCrypto
                                                                    , runBeam
                                                                    , userEventAttendancePivot
                                                                    )
import           Infra.Beam.Schema.Latest                           ( EventEntity
                                                                    , EventEntityT(..)
                                                                    , PrimaryKey(..)
                                                                    , UserEntityId
                                                                    , UserEventAttendancePivotT(..)
                                                                    )
import           RIO                                                ( ($)
                                                                    , (<$>)
                                                                    , (<>)
                                                                    , (==)
                                                                    , Maybe(..)
                                                                    , MonadIO
                                                                    , catMaybes
                                                                    , fst
                                                                    , map
                                                                    , mapMaybe
                                                                    , null
                                                                    , on
                                                                    , return
                                                                    , snd
                                                                    )
import           RIO.List                                           ( nubBy )
import           RIO.List.Partial                                   ( head )
import           Utils                                              ( toMaybe )


-- TODO return only those that are in future, (or sorted, or other kind of query param)
-- TODO preferrably using sort of argumetn
allEvents :: (Has Connection e, Has Config e, MonadIO m) => e -> m [(EventEntity, [UserEntityId])]
allEvents e = runBeam e $ do
  eventsPivots <- runSelectReturningList $ select $ do
    event <- all_ eventsTable
    pivot <- leftJoin_ (all_ userEventAttendancePivot) (\p -> ueapEventId p `references_` event)
    return (event, pivot)
  let events = nubBy ((==) `on` eeId) (fst <$> eventsPivots)
  let pivots = catMaybes (snd <$> eventsPivots)
  let attendeesAt ev = mapMaybe (\p -> toMaybe (ueapEventId p == pk ev) (ueapUserId p)) pivots -- TODO use `aggregate_` function?
  return [ (event, attendeesAt event) | event <- events ]

maybeEventById
  :: (Has Connection e, Has Config e, MonadIO m)
  => e
  -> UUID
  -> m (Maybe EventEntity, [UserEntityId])
maybeEventById e eventId = do
  eventsPivots <- runBeam e $ runSelectReturningList $ select $ do
    event <- filter_ (\ev -> pk ev ==. val_ (EventEntityId eventId)) (all_ eventsTable)
    pivot <- leftJoin_ (all_ userEventAttendancePivot)
                       (\p -> ueapEventId p ==. val_ (EventEntityId eventId))
    return (event, pivot)

  if null eventsPivots
    then return (Nothing, [])
    else do
      let event       = head $ fst <$> eventsPivots
      let attendeeIds = map ueapUserId (catMaybes $ snd <$> eventsPivots)
      return (Just event, attendeeIds)

createAndInsertEvent
  :: (Has Connection e, Has Config e, MonadIO m) => e -> NewEventData -> m EventEntity
createAndInsertEvent e NewEventData {..} = runBeam e $ do
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

updateEventDetails :: (Has Connection e, Has Config e, MonadIO m) => e -> Event -> m ()
updateEventDetails e Event {..} = runBeam e $ runUpdate $ update
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

-- TODO do I need to remove stuff from pivot table?
deleteEvent :: (Has Connection e, Has Config e, MonadIO m) => e -> UUID -> m ()
deleteEvent e eId =
  runBeam e $ runDelete $ delete eventsTable (\EventEntity {..} -> eeId ==. val_ eId)

attendAtEvent :: (Has Connection e, Has Config e, MonadIO m) => e -> Event -> UUID -> m ()
attendAtEvent e event userId =
  runBeam e $ runInsert $ insert userEventAttendancePivot $ insertExpressions
    [ UserEventAttendancePivot { ueapUserId    = val_ (UserEntityId userId)
                               , ueapEventId   = val_ (EventEntityId (eId event))
                               , ueapCreatedAt = currentTimestamp_
                               }
    ]
