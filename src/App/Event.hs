module App.Event
  ( getEvents
  , createNewEvent
  , findEventById
  , updateEventDetails
  , deleteEvent
  , attendEvent
  , unattendEvent
  ) where

import qualified Domain.App.Class                                  as C
import           Domain.App.Types                                   ( EventId )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Event                                       ( Action(..)
                                                                    , Event(..)
                                                                    )
import           Domain.Event.EventData                             ( NewEventData
                                                                    , UpdateEventInfoData(..)
                                                                    )
import           RIO                                                ( (>>)
                                                                    , (>>=)
                                                                    )


getEvents :: (C.AccessPolicyGuard m, C.EventRepository m) => UserClaims -> m [Event]
getEvents uc = C.checkPolicy uc GetAllEvents >> C.getAllEvents

createNewEvent
  :: (C.AccessPolicyGuard m, C.EventRepository m) => UserClaims -> NewEventData -> m Event
createNewEvent uc eData = C.checkPolicy uc CreateEvent >> C.createEvent eData

findEventById :: (C.AccessPolicyGuard m, C.EventRepository m) => UserClaims -> EventId -> m Event
findEventById uc eid = C.checkPolicy uc GetEvent >> C.getEventById eid

updateEventDetails
  :: (C.AccessPolicyGuard m, C.EventRepository m)
  => UserClaims
  -> EventId
  -> UpdateEventInfoData
  -> m Event
updateEventDetails uc eid UpdateEventInfoData {..} = C.getEventById eid >>= \e ->
  C.checkPolicy uc (UpdateEventInfo (eCreatedBy e)) >> C.saveEvent e
    { eTitle         = ueidTitle
    , eDescription   = ueidDescription
    , eLastUpdatedBy = ueidLastUpdatedBy
    , eStart         = ueidStart
    , eEnd           = ueidEnd
    }

deleteEvent :: (C.AccessPolicyGuard m, C.EventRepository m) => UserClaims -> EventId -> m ()
deleteEvent uc eid = C.getEventById eid >>= \e -> do
  C.checkPolicy uc (DeleteEvent (eCreatedBy e)) >> C.deleteEvent eid

attendEvent :: (C.AccessPolicyGuard m, C.EventRepository m) => UserClaims -> EventId -> m ()
attendEvent uc eid = C.getEventById eid >>= \e -> do
  C.checkPolicy uc (AttendEvent (ucId uc)) >> C.attendEvent e (ucId uc)

unattendEvent :: (C.AccessPolicyGuard m, C.EventRepository m) => UserClaims -> EventId -> m ()
unattendEvent uc eid = C.getEventById eid >>= \e -> do
  C.checkPolicy uc (UnattendEvent (ucId uc)) >> C.unattendEvent e (ucId uc)
