module Api.Mapper
  ( userToUserDto
  , eventToEventDto
  , signupDtoToUserData
  , newEventDtoToEventData
  , updateEventInfoDtoToEventData
  ) where

import           Api.Resources.Auth                                 ( SignupDto(..) )
import           Api.Resources.Event                                ( EventDto(..)
                                                                    , NewEventDto(..)
                                                                    , UpdateEventInfoDto(..)
                                                                    )
import           Api.Resources.User                                 ( UserDto(..) )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Event                                       ( Event(..) )
import           Domain.Event.EventData                             ( NewEventData(..)
                                                                    , UpdateEventInfoData(..)
                                                                    )
import           Domain.User                                        ( User(..) )
import           Domain.User.UserData                               ( NewUserData(..) )


userToUserDto :: User -> UserDto
userToUserDto User {..} = UserDto { uDtoId        = uId
                                  , uDtoUsername  = uUsername
                                  , uDtoFirstName = uFirstName
                                  , uDtoLastName  = uLastName
                                  }

signupDtoToUserData :: SignupDto -> NewUserData
signupDtoToUserData SignupDto {..} = NewUserData { nudUsername  = sDtoUsername
                                                 , nudPassword  = sDtoPassword
                                                 , nudRoles     = [Participant] -- TODO deal with roles adjustment
                                                 , nudFirstName = sDtoFirstName
                                                 , nudLastName  = sDtoLastName
                                                 }

eventToEventDto :: Event -> EventDto
eventToEventDto Event {..} = EventDto { eDtoId            = eId
                                      , eDtoTitle         = eTitle
                                      , eDtoDescription   = eDescription
                                      , eDtoCreatedBy     = eCreatedBy
                                      , eDtoLastUpdatedBy = eLastUpdatedBy
                                      , eDtoAttendees     = eAttendees
                                      , eDtoStart         = eStart
                                      , eDtoEnd           = eEnd
                                      }

newEventDtoToEventData :: NewEventDto -> UserClaims -> NewEventData
newEventDtoToEventData NewEventDto {..} UserClaims {..} = NewEventData
  { nedTitle         = neDtoTitle
  , nedDescription   = neDtoDescription
  , nedCreatedBy     = ucId
  , nedLastUpdatedBy = ucId
  , nedStart         = neDtoStart
  , nedEnd           = neDtoEnd
  }

updateEventInfoDtoToEventData :: UpdateEventInfoDto -> UserClaims -> UpdateEventInfoData
updateEventInfoDtoToEventData UpdateEventInfoDto {..} UserClaims {..} = UpdateEventInfoData
  { ueidTitle         = ueiDtoTitle
  , ueidDescription   = ueiDtoDescription
  , ueidLastUpdatedBy = ucId
  , ueidStart         = ueiDtoStart
  , ueidEnd           = ueiDtoEnd
  }
