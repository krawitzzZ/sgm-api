module Domain.Event.EventData
  ( NewEventData(..)
  , UpdateEventInfoData(..)
  ) where

import           Data.UUID                                ( UUID )
import           RIO                                      ( Maybe
                                                          , Text
                                                          )
import           RIO.Time                                 ( LocalTime )


data NewEventData = NewEventData
  { nedTitle         :: !Text
  , nedDescription   :: !(Maybe Text)
  , nedCreatedBy     :: !UUID
  , nedLastUpdatedBy :: !UUID
  , nedStart         :: !LocalTime
  , nedEnd           :: !LocalTime
  }

data UpdateEventInfoData = UpdateEventInfoData
  { ueidTitle         :: !Text
  , ueidDescription   :: !(Maybe Text)
  , ueidLastUpdatedBy :: !UUID
  , ueidStart         :: !LocalTime
  , ueidEnd           :: !LocalTime
  }
