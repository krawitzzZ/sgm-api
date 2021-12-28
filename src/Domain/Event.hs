module Domain.Event
  ( Event(..)
  , Action(..)
  ) where

import           Data.Time                                          ( LocalTime )
import           Domain.App.Types                                   ( CreatedBy
                                                                    , EventId
                                                                    , UserId
                                                                    )
import           Domain.Auth.Permission                             ( Permission(..)
                                                                    , check
                                                                    )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Policy.AccessPolicy                         ( AccessPolicy(..)
                                                                    , Action
                                                                    )
import           RIO                                                ( (<>)
                                                                    , (==)
                                                                    , Eq
                                                                    , Maybe
                                                                    , Text
                                                                    , on
                                                                    )
import           Utils                                              ( anyElem )


data Event = Event
  { eId            :: !EventId
  , eTitle         :: !Text
  , eDescription   :: !(Maybe Text)
  , eCreatedBy     :: !UserId
  , eLastUpdatedBy :: !UserId
  , eAttendees     :: ![UserId]
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
    UpdateEventInfo CreatedBy |
    DeleteEvent CreatedBy |
    AttendEvent EventId |
    UnattendEvent EventId

  checkAccessPolicy _ CreateEvent  = Granted
  checkAccessPolicy _ GetEvent     = Granted
  checkAccessPolicy _ GetAllEvents = Granted
  checkAccessPolicy UserClaims {..} (UpdateEventInfo createdBy) =
    check (ucId == createdBy) <> check ([Moderator, Admin, Superadmin] `anyElem` ucRoles)
  checkAccessPolicy UserClaims {..} (DeleteEvent createdBy) =
    check (ucId == createdBy) <> check ([Moderator, Admin, Superadmin] `anyElem` ucRoles)
  checkAccessPolicy _ (AttendEvent   _) = Granted
  checkAccessPolicy _ (UnattendEvent _) = Granted
