module Domain.Event.EventData
  ( NewEventData(..)
  , UpdateEventInfoData(..)
  ) where

import           Domain.App.Types                                   ( UserId )
import           RIO                                                ( Eq
                                                                    , Maybe
                                                                    , Show
                                                                    , Text
                                                                    )
import           RIO.Time                                           ( LocalTime )


data NewEventData = NewEventData
  { nedTitle         :: !Text
  , nedDescription   :: !(Maybe Text)
  , nedCreatedBy     :: !UserId
  , nedLastUpdatedBy :: !UserId
  , nedStart         :: !LocalTime
  , nedEnd           :: !LocalTime
  }
  deriving (Eq, Show)

data UpdateEventInfoData = UpdateEventInfoData
  { ueidTitle         :: !Text
  , ueidDescription   :: !(Maybe Text)
  , ueidLastUpdatedBy :: !UserId
  , ueidStart         :: !LocalTime
  , ueidEnd           :: !LocalTime
  }
  deriving (Eq, Show)
