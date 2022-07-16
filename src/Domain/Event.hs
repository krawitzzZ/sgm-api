module Domain.Event
  ( Event(..)
  , Action(..)
  ) where

import           Data.Time                                          ( LocalTime )
import           Domain.App.Types                                   ( EventId
                                                                    , UserId
                                                                    )
import           Domain.Auth.Permission                             ( Permission(..)
                                                                    , check
                                                                    , isPermitted
                                                                    )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Policy                                      ( Action
                                                                    , HasActionPolicy(..)
                                                                    )
import           RIO                                                ( (.)
                                                                    , (<>)
                                                                    , (==)
                                                                    , Eq
                                                                    , Maybe
                                                                    , Show
                                                                    , Text
                                                                    , filter
                                                                    , map
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
  deriving Show

instance Eq Event where
  (==) = (==) `on` eId

instance HasActionPolicy Event where
  data Action Event =
    CreateEvent |
    GetEvent |
    GetAllEvents |
    UpdateEventInfo UserId |
    DeleteEvent UserId |
    AttendEvent UserId |
    UnattendEvent UserId
    deriving (Eq, Show)

  actionPermission _ CreateEvent  = Granted
  actionPermission _ GetEvent     = Granted
  actionPermission _ GetAllEvents = Granted
  actionPermission UserClaims {..} (UpdateEventInfo createdBy) =
    check (ucId == createdBy) <> check ([Moderator, Admin, Superadmin] `anyElem` ucRoles)
  actionPermission UserClaims {..} (DeleteEvent createdBy) =
    check (ucId == createdBy) <> check ([Moderator, Admin, Superadmin] `anyElem` ucRoles)
  actionPermission _ (AttendEvent   _) = Granted
  actionPermission _ (UnattendEvent _) = Granted

  permittedActions uc Event {..} = filter (permitted uc) allActions
   where
    permitted c = isPermitted . actionPermission c
    allActions =
      [CreateEvent, GetEvent, GetAllEvents, UpdateEventInfo eCreatedBy, DeleteEvent eCreatedBy]
        <> map AttendEvent   eAttendees
        <> map UnattendEvent eAttendees
