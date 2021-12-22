module App.Event
  ( getEvents
  , createNewEvent
  , findEventById
  , updateEventDetails
  , deleteEvent
  ) where

import           Data.UUID                                ( UUID )
import           Domain.Auth                              ( AuthUser )
import qualified Domain.Class                            as C
import           Domain.Event                             ( Event(..)
                                                          , NewEventData
                                                          , UpdateEventInfoData(..)
                                                          )
import           RIO                                      ( (>>=) )


getEvents :: (C.EventRepository m) => m [Event]
getEvents = C.getAllEvents

createNewEvent :: (C.EventRepository m) => NewEventData -> AuthUser -> m Event
createNewEvent eventData _ = C.createEvent eventData -- TODO use Policy

findEventById :: (C.EventRepository m) => UUID -> m Event
findEventById = C.getEventById

-- TODO use Policy
updateEventDetails :: (C.EventRepository m) => UUID -> UpdateEventInfoData -> AuthUser -> m Event
updateEventDetails eventId UpdateEventInfoData {..} _ = C.getEventById eventId >>= \event ->
  C.saveEvent event { eTitle         = ueidTitle
                    , eDescription   = ueidDescription
                    , eLastUpdatedBy = ueidLastUpdatedBy
                    , eStart         = ueidStart
                    , eEnd           = ueidEnd
                    }

-- TODO use Policy
deleteEvent :: (C.EventRepository m) => UUID -> AuthUser -> m ()
deleteEvent id _ = C.deleteEvent id
