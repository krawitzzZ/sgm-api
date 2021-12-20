module Domain.Event
  ( Event(..)
  , EventData(..)
  , EventRepository(..)
  ) where

import           Data.Time                                ( LocalTime )
import           Data.UUID                                ( UUID )
import           RIO                                      ( Eq
                                                          , Generic
                                                          , Maybe
                                                          , Show
                                                          , Text
                                                          )


data Event = Event
  { eId            :: !UUID
  , eTitle         :: !Text
  , eDescription   :: !(Maybe Text)
  , eCreatedBy     :: !UUID
  , eLastUpdatedBy :: !UUID
  , eAttendees     :: ![UUID]
  , eStart         :: !LocalTime
  , eEnd           :: !LocalTime
  }
  deriving (Eq, Show, Generic)

data EventData = EventData
  { edTitle       :: !Text
  , edDescription :: !(Maybe Text)
  , edStart       :: !LocalTime
  , edEnd         :: !LocalTime
  }
  deriving (Eq, Show, Generic)

class EventRepository m where
  getEventById :: UUID -> m Event
  getAllEvents :: m [Event]
  createEvent :: EventData -> m Event
  saveEvent :: Event -> m ()
  deleteEvent :: UUID -> m ()
