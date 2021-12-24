module App.Event
  ( getEvents
  , createNewEvent
  , findEventById
  , updateEventDetails
  , deleteEvent
  ) where

import           Control.Exception.Safe                   ( MonadThrow )
import           Data.UUID                                ( UUID )
import qualified Domain.App.Class                        as C
import           Domain.Auth.UserClaims                   ( UserClaims )
import           Domain.Event                             ( Action(..)
                                                          , Event(..)
                                                          )
import           Domain.Event.EventData                   ( NewEventData
                                                          , UpdateEventInfoData(..)
                                                          )
import           Domain.Policy                            ( accessPolicyGuard )
import           RIO                                      ( (>>)
                                                          , (>>=)
                                                          )


getEvents :: (C.EventRepository m, MonadThrow m) => UserClaims -> m [Event]
getEvents me = accessPolicyGuard me GetAllEvents >> C.getAllEvents

createNewEvent :: (C.EventRepository m, MonadThrow m) => UserClaims -> NewEventData -> m Event
createNewEvent me eventData = accessPolicyGuard me CreateEvent >> C.createEvent eventData

findEventById :: (C.EventRepository m, MonadThrow m) => UserClaims -> UUID -> m Event
findEventById me eventId = accessPolicyGuard me GetEvent >> C.getEventById eventId

updateEventDetails
  :: (C.EventRepository m, MonadThrow m) => UserClaims -> UUID -> UpdateEventInfoData -> m Event
updateEventDetails me eventId UpdateEventInfoData {..} = C.getEventById eventId >>= \event -> do
  accessPolicyGuard me (UpdateEventInfo (eCreatedBy event)) >> C.saveEvent event
    { eTitle         = ueidTitle
    , eDescription   = ueidDescription
    , eLastUpdatedBy = ueidLastUpdatedBy
    , eStart         = ueidStart
    , eEnd           = ueidEnd
    }

deleteEvent :: (C.EventRepository m, MonadThrow m) => UserClaims -> UUID -> m ()
deleteEvent me eventId = C.getEventById eventId >>= \event -> do
  accessPolicyGuard me (DeleteEvent (eCreatedBy event)) >> C.deleteEvent eventId
