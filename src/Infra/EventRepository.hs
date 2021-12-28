module Infra.EventRepository
  ( getAll
  , findOneById
  , createOne
  , saveOne
  , deleteOne
  , attendOne
  ) where

import           Control.Exception.Safe                             ( MonadCatch
                                                                    , throwM
                                                                    )
import           Control.Monad.Reader.Has                           ( Has )
import           Data.String.Conversions                            ( cs )
import           Data.UUID                                          ( UUID
                                                                    , toText
                                                                    )
import           Database.Beam.Postgres                             ( Connection )
import           Domain.App.Config                                  ( Config )
import           Domain.Event                                       ( Event(..) )
import           Domain.Event.EventData                             ( NewEventData )
import           Domain.Exception                                   ( DomainException(..) )
import           Infra.Beam.Exception                               ( BeamException(..) )
import           Infra.Beam.Mapper                                  ( eventEntityToDomain )
import           Infra.Beam.Query.Event                             ( allEvents
                                                                    , attendAtEvent
                                                                    , createAndInsertEvent
                                                                    , deleteEvent
                                                                    , maybeEventById
                                                                    , updateEventDetails
                                                                    )
import           Infra.Exception                                    ( tryCatchBeam
                                                                    , tryCatchBeamDefault
                                                                    )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<&>)
                                                                    , (<>)
                                                                    , (>>)
                                                                    , (>>=)
                                                                    , Maybe(..)
                                                                    , MonadIO
                                                                    , map
                                                                    , return
                                                                    , show
                                                                    , uncurry
                                                                    )


-- TODO get by sorting, time, etc
getAll :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> m [Event]
getAll c = do
  tryCatchBeamDefault $ allEvents c <&> map (uncurry eventEntityToDomain)

findOneById :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> UUID -> m Event
findOneById e eventId = tryCatchBeamDefault $ maybeEventById e eventId >>= \case
  (Nothing   , _  ) -> throwM . NotFound $ "Event with id '" <> toText eventId <> "' not found"
  (Just event, ids) -> return $ eventEntityToDomain event ids

createOne
  :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> NewEventData -> m Event
createOne e event =
  tryCatchBeamDefault $ createAndInsertEvent e event <&> (`eventEntityToDomain` [])

saveOne :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> Event -> m Event
saveOne e event = tryCatchBeamDefault $ updateEventDetails e event >> return event

deleteOne :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> UUID -> m ()
deleteOne e eventId = tryCatchBeamDefault $ maybeEventById e eventId >>= \case
  (Nothing, _) -> throwM . NotFound $ "Event with id '" <> toText eventId <> "' not found"
  (Just _ , _) -> deleteEvent e eventId

attendOne
  :: (Has Connection e, Has Config e, MonadCatch m, MonadIO m) => e -> Event -> UUID -> m ()
attendOne e event userId = tryCatchBeam attend handleBeamException
 where
  attend = maybeEventById e (eId event) >>= \case
    (Nothing, _) -> throwM . NotFound $ "Event with id '" <> toText (eId event) <> "' not found"
    (Just _ , _) -> attendAtEvent e event userId
  handleBeamException = \case
    (Conflict _) -> return ()
    err          -> throwM . InternalError . cs . show $ err
