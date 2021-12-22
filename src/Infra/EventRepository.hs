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
import           Domain.Class                             ( MonadLogger(..) )
import           Domain.Event                             ( Event(..)
                                                          , NewEventData(..)
                                                          )
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
                                                          , MonadReader
                                                          , map
                                                          , return
                                                          , show
                                                          )


getAll :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => m [Event]
getAll = withContext (mkContext "getAll") $ tryCatchDefault $ allEvents <&> map eventEntityToDomain

findOneById
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => UUID -> m Event
findOneById id =
  withContext (mkContext "findOneById") $ tryCatchDefault $ maybeEventById id >>= \case
    Just event -> return $ eventEntityToDomain event
    Nothing    -> throwM . NotFound $ "Event with id '" <> cs (show id) <> "' not found"

createOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m)
  => NewEventData
  -> m Event
createOne event =
  withContext (mkContext "createOne")
    $   tryCatchDefault
    $   createAndInsertEvent event
    <&> eventEntityToDomain

saveOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => Event -> m Event
saveOne event =
  withContext (mkContext "saveOne") $ tryCatchDefault $ updateEventDetails event >> return event

deleteOne
  :: (Has Connection e, MonadReader e m, MonadLogger m, MonadCatch m, MonadIO m) => UUID -> m ()
deleteOne = withContext (mkContext "deleteOne") . tryCatchDefault . deleteEvent


mkContext :: LogContext -> LogContext
mkContext = ("Infra.EventRepository->" <>)
