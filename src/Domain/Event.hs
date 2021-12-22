module Domain.Event
  ( Event(..)
  , NewEventData(..)
  , UpdateEventInfoData(..)
  ) where

import           Data.Time                                ( LocalTime )
import           Data.UUID                                ( UUID )
import           RIO                                      ( (==)
                                                          , Eq
                                                          , Generic
                                                          , Maybe
                                                          , Text
                                                          , on
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
  deriving Generic

instance Eq Event where
  (==) = (==) `on` eId

data NewEventData = NewEventData
  { nedTitle         :: !Text
  , nedDescription   :: !(Maybe Text)
  , nedCreatedBy     :: !UUID
  , nedLastUpdatedBy :: !UUID
  , nedStart         :: !LocalTime
  , nedEnd           :: !LocalTime
  }
  deriving Generic

data UpdateEventInfoData = UpdateEventInfoData
  { ueidTitle         :: !Text
  , ueidDescription   :: !(Maybe Text)
  , ueidLastUpdatedBy :: !UUID
  , ueidStart         :: !LocalTime
  , ueidEnd           :: !LocalTime
  }
  deriving Generic
