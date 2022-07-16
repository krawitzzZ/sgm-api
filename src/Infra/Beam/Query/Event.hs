module Infra.Beam.Query.Event
  ( allEvents
  , maybeEventById
  , createAndInsertEvent
  , updateEventDetails
  , deleteEvent
  , attendAtEvent
  , unattendAtEvent
  ) where

import           Control.Monad.Reader.Has                           ( Has )
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
import           Domain.App.Types                                   ( EventId(..)
                                                                    , UserId(..)
                                                                    )
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
  -> EventId
  -> m (Maybe EventEntity, [UserEntityId])
maybeEventById e (EventId eventId) = do
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
                  , eeCreatedBy     = val_ (UserEntityId $ unUserId nedCreatedBy)
                  , eeLastUpdatedBy = val_ (UserEntityId $ unUserId nedCreatedBy)
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
      <> (eeLastUpdatedBy <-. val_ (UserEntityId $ unUserId eLastUpdatedBy))
      <> (eeStart <-. val_ eStart)
      <> (eeEnd <-. val_ eEnd)
  )
  (\EventEntity {..} -> eeId ==. val_ (unEventId eId))

deleteEvent :: (Has Connection e, Has Config e, MonadIO m) => e -> EventId -> m ()
deleteEvent e (EventId eventId) =
  runBeam e $ runDelete $ delete eventsTable (\EventEntity {..} -> eeId ==. val_ eventId)

attendAtEvent :: (Has Connection e, Has Config e, MonadIO m) => e -> Event -> UserId -> m ()
attendAtEvent e event (UserId userId) =
  runBeam e $ runInsert $ insert userEventAttendancePivot $ insertExpressions
    [ UserEventAttendancePivot { ueapUserId    = val_ (UserEntityId userId)
                               , ueapEventId   = val_ (EventEntityId (unEventId $ eId event))
                               , ueapCreatedAt = currentTimestamp_
                               }
    ]

unattendAtEvent :: (Has Connection e, Has Config e, MonadIO m) => e -> Event -> UserId -> m ()
unattendAtEvent e event (UserId userId) = runBeam e $ runDelete $ delete
  userEventAttendancePivot
  (\p -> pk p ==. val_
    (UserEventAttendancePivotId (UserEntityId userId) (EventEntityId (unEventId $ eId event)))
  )
