module App.Event
  ( getEvents
  , createNewEvent
  , findEventById
  , updateEventDetails
  , deleteEvent
  , attendEvent
  ) where

import           Control.Exception.Safe                             ( MonadThrow )
import           Data.UUID                                          ( UUID )
import qualified Domain.App.Class                                  as C
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Event                                       ( Action(..)
                                                                    , Event(..)
                                                                    )
import           Domain.Event.EventData                             ( NewEventData
                                                                    , UpdateEventInfoData(..)
                                                                    )
import           Domain.Policy                                      ( accessPolicyGuard )
import           RIO                                                ( (>>)
                                                                    , (>>=)
                                                                    )


getEvents :: (C.EventRepository m, MonadThrow m) => UserClaims -> m [Event]
getEvents claims = accessPolicyGuard claims GetAllEvents >> C.getAllEvents

createNewEvent :: (C.EventRepository m, MonadThrow m) => UserClaims -> NewEventData -> m Event
createNewEvent claims eventData = accessPolicyGuard claims CreateEvent >> C.createEvent eventData

findEventById :: (C.EventRepository m, MonadThrow m) => UserClaims -> UUID -> m Event
findEventById claims eventId = accessPolicyGuard claims GetEvent >> C.getEventById eventId

updateEventDetails
  :: (C.EventRepository m, MonadThrow m) => UserClaims -> UUID -> UpdateEventInfoData -> m Event
updateEventDetails claims eventId UpdateEventInfoData {..} = C.getEventById eventId >>= \event ->
  accessPolicyGuard claims (UpdateEventInfo (eCreatedBy event)) >> C.saveEvent event
    { eTitle         = ueidTitle
    , eDescription   = ueidDescription
    , eLastUpdatedBy = ueidLastUpdatedBy
    , eStart         = ueidStart
    , eEnd           = ueidEnd
    }

deleteEvent :: (C.EventRepository m, MonadThrow m) => UserClaims -> UUID -> m ()
deleteEvent claims eventId = C.getEventById eventId >>= \event -> do
  accessPolicyGuard claims (DeleteEvent (eCreatedBy event)) >> C.deleteEvent eventId

attendEvent :: (C.EventRepository m, MonadThrow m) => UserClaims -> UUID -> m ()
attendEvent claims eventId = C.getEventById eventId >>= \event -> do
  accessPolicyGuard claims (AttendEvent eventId) >> C.attendEvent event (ucId claims)
