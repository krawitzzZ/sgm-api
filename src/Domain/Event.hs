module Domain.Event
  ( Event(..)
  , Action(..)
  ) where

import           Data.Time                                ( LocalTime )
import           Data.UUID                                ( UUID )
import           Domain.Auth.Permission                   ( Permission(..)
                                                          , check
                                                          )
import           Domain.Auth.Role                         ( Role(..) )
import           Domain.Auth.UserClaims                   ( UserClaims(..) )
import           Domain.Policy.AccessPolicy               ( AccessPolicy(..)
                                                          , Action
                                                          )
import           RIO                                      ( (<>)
                                                          , (==)
                                                          , Eq
                                                          , Maybe
                                                          , Text
                                                          , on
                                                          )
import           Utils                                    ( anyElem )


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

instance Eq Event where
  (==) = (==) `on` eId

instance AccessPolicy Event where
  data Action Event =
    CreateEvent |
    GetEvent |
    GetAllEvents |
    UpdateEventInfo UUID |
    DeleteEvent UUID

  checkAccessPolicy _ CreateEvent  = Granted
  checkAccessPolicy _ GetEvent     = Granted
  checkAccessPolicy _ GetAllEvents = Granted
  checkAccessPolicy UserClaims {..} (UpdateEventInfo createdBy) =
    check (ucId == createdBy) <> check ([Moderator, Admin, Superadmin] `anyElem` ucRoles)
  checkAccessPolicy UserClaims {..} (DeleteEvent createdBy) =
    check (ucId == createdBy) <> check ([Moderator, Admin, Superadmin] `anyElem` ucRoles)
