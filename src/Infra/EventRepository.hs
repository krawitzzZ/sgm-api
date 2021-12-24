module Infra.EventRepository
  ( getAll
  , findOneById
  , createOne
  , saveOne
  , deleteOne
  ) where

import           Control.Exception.Safe                   ( MonadCatch
                                                          , throwM
                                                          )
import           Control.Monad.Reader.Has                 ( Has )
import           Data.String.Conversions                  ( cs )
import           Data.UUID                                ( UUID )
import           Database.Beam.Postgres                   ( Connection )
import           Domain.App.Class                         ( MonadLogger(..) )
import           Domain.Event                             ( Event(..) )
import           Domain.Event.EventData                   ( NewEventData )
import           Domain.Exception                         ( DomainException(..) )
import           Domain.Logger                            ( LogContext )
import           Infra.Beam.Mapper                        ( eventEntityToDomain )
import           Infra.Beam.Query.Event                   ( allEvents
                                                          , createAndInsertEvent
                                                          , deleteEvent
                                                          , maybeEventById
                                                          , updateEventDetails
                                                          )
import           Infra.Exception                          ( tryCatchDefault )
import           RIO                                      ( ($)
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
                                                          )


getAll :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> m [Event]
getAll c =
  withContext (mkContext "getAll") $ tryCatchDefault $ allEvents c <&> map eventEntityToDomain

findOneById :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> UUID -> m Event
findOneById c eId =
  withContext (mkContext "findOneById") $ tryCatchDefault $ maybeEventById c eId >>= \case
    Just event -> return $ eventEntityToDomain event
    Nothing    -> throwM . NotFound $ "Event with id '" <> cs (show eId) <> "' not found"

createOne
  :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> NewEventData -> m Event
createOne c event =
  withContext (mkContext "createOne")
    $   tryCatchDefault
    $   createAndInsertEvent c event
    <&> eventEntityToDomain

saveOne :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> Event -> m Event
saveOne c event =
  withContext (mkContext "saveOne") $ tryCatchDefault $ updateEventDetails c event >> return event

deleteOne :: (Has Connection c, MonadLogger m, MonadCatch m, MonadIO m) => c -> UUID -> m ()
deleteOne c = withContext (mkContext "deleteOne") . tryCatchDefault . deleteEvent c


mkContext :: LogContext -> LogContext
mkContext = ("Infra.EventRepository->" <>)
