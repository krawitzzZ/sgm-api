module Api.Mapper
  ( userToUserDto
  , eventToEventDto
  , signupDtoToUserData
  , newEventDtoToEventData
  , updateEventInfoDtoToEventData
  ) where

import           Api.Resources.Auth                       ( SignupDto(..) )
import           Api.Resources.Event                      ( EventDto(..)
                                                          , NewEventDto(..)
                                                          , UpdateEventInfoDto(..)
                                                          )
import           Api.Resources.User                       ( UserDto(..) )
import           Domain.Auth                              ( AuthUser(..) )
import           Domain.Event                             ( Event(..)
                                                          , NewEventData(..)
                                                          , UpdateEventInfoData(..)
                                                          )
import           Domain.Role                              ( Role(..) )
import           Domain.User                              ( NewUserData(..)
                                                          , User(..)
                                                          )


userToUserDto :: User -> UserDto
userToUserDto User {..} = UserDto { uDtoId        = uId
                                  , uDtoUsername  = uUsername
                                  , uDtoFirstName = uFirstName
                                  , uDtoLastName  = uLastName
                                  }

signupDtoToUserData :: SignupDto -> NewUserData
signupDtoToUserData SignupDto {..} = NewUserData { nudUsername  = sDtoUsername
                                                 , nudPassword  = sDtoPassword
                                                 , nudRoles     = [Participant]
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

newEventDtoToEventData :: NewEventDto -> AuthUser -> NewEventData
newEventDtoToEventData NewEventDto {..} AuthUser {..} = NewEventData
  { nedTitle         = neDtoTitle
  , nedDescription   = neDtoDescription
  , nedCreatedBy     = auId
  , nedLastUpdatedBy = auId
  , nedStart         = neDtoStart
  , nedEnd           = neDtoEnd
  }

updateEventInfoDtoToEventData :: UpdateEventInfoDto -> AuthUser -> UpdateEventInfoData
updateEventInfoDtoToEventData UpdateEventInfoDto {..} AuthUser {..} = UpdateEventInfoData
  { ueidTitle         = ueiDtoTitle
  , ueidDescription   = ueiDtoDescription
  , ueidLastUpdatedBy = auId
  , ueidStart         = ueiDtoStart
  , ueidEnd           = ueiDtoEnd
  }
