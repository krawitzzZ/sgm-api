module TestUtils
  ( mkUUID
  , mkUser
  , mkEvent
  , mkNewEventData
  , mkUpdateEventInfoData
  ) where

import qualified Data.Password.Argon2                              as P
import           Data.UUID                                          ( UUID
                                                                    , fromString
                                                                    , nil
                                                                    )
import           Domain.App.Types                                   ( EventId(..)
                                                                    , UserId(..)
                                                                    )
import           Domain.Auth.Password                               ( PasswordHash(..) )
import           Domain.Auth.Role                                   ( Role )
import           Domain.Event                                       ( Event(..) )
import           Domain.Event.EventData                             ( NewEventData(..)
                                                                    , UpdateEventInfoData(..)
                                                                    )
import           Domain.User                                        ( User(..) )
import           RIO                                                ( ($)
                                                                    , Maybe(..)
                                                                    , Text
                                                                    , fromMaybe
                                                                    )
import           RIO.Text                                           ( unpack )
import           RIO.Time                                           ( UTCTime
                                                                    , utc
                                                                    , utcToLocalTime
                                                                    )


mkUUID :: Text -> UUID
mkUUID t = fromMaybe nil $ fromString (unpack t)

mkUser :: UserId -> [Role] -> User
mkUser uid roles = User { uId        = uid
                        , uUsername  = "user"
                        , uPassword  = PasswordHash (P.PasswordHash "password")
                        , uRoles     = roles
                        , uFirstName = Just "name"
                        , uLastName  = Just "surname"
                        }

mkEvent :: EventId -> UserId -> UTCTime -> Event
mkEvent eid uid time = Event { eId            = eid
                             , eTitle         = "event"
                             , eDescription   = Nothing
                             , eCreatedBy     = uid
                             , eLastUpdatedBy = uid
                             , eAttendees     = []
                             , eStart         = utcToLocalTime utc time
                             , eEnd           = utcToLocalTime utc time
                             }

mkNewEventData :: Text -> UserId -> UTCTime -> NewEventData
mkNewEventData nedTitle uid time = NewEventData { nedTitle
                                                , nedDescription   = Nothing
                                                , nedCreatedBy     = uid
                                                , nedLastUpdatedBy = uid
                                                , nedStart         = utcToLocalTime utc time
                                                , nedEnd           = utcToLocalTime utc time
                                                }


mkUpdateEventInfoData :: Text -> UserId -> UTCTime -> UpdateEventInfoData
mkUpdateEventInfoData ueidTitle uid time = UpdateEventInfoData
  { ueidTitle
  , ueidDescription   = Nothing
  , ueidLastUpdatedBy = uid
  , ueidStart         = utcToLocalTime utc time
  , ueidEnd           = utcToLocalTime utc time
  }
