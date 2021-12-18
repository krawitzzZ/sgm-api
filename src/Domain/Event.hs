module Domain.Event
  ( Event(..)
  , EventData(..)
  , EventRepository(..)
  ) where

import           Data.Time                                ( LocalTime )
import           Data.UUID                                ( UUID )
import           Domain.User                              ( User )
import           RIO                                      ( Eq
                                                          , Generic
                                                          , Maybe
                                                          , Show
                                                          , Text
                                                          )


data Event = Event
  { eventId            :: !UUID
  , eventTitle         :: !Text
  , eventDescription   :: !(Maybe Text)
  , eventCreatedBy     :: !User
  , eventLastUpdatedBy :: !User
  , eventAttendees     :: ![User]
  , eventStart         :: !LocalTime
  , eventEnd           :: !LocalTime
  }
  deriving (Eq, Show, Generic)

data EventData = EventData
  { eventDataTitle       :: !Text
  , eventDataDescription :: !(Maybe Text)
  , eventDataStart       :: !LocalTime
  , eventDataEnd         :: !LocalTime
  }
  deriving (Eq, Show, Generic)

class EventRepository m where
  getEventById :: UUID -> m Event
  getAllEvents :: m [Event]
  createEvent :: EventData -> m Event
  saveEvent :: Event -> m ()
  deleteEvent :: UUID -> m ()
