module Api.Mapper
  ( userToUserDto
  , signupDtoToUserData
  ) where

import           Api.Resources.Auth                       ( SignupDto(..) )
import           Api.Resources.User                       ( UserDto(..) )
import           Domain.User                              ( NewUserData(..)
                                                          , User(..)
                                                          )


userToUserDto :: User -> UserDto
userToUserDto User { uId, uUsername, uFirstName, uLastName } = UserDto { uDtoId        = uId
                                                                       , uDtoUsername  = uUsername
                                                                       , uDtoFirstName = uFirstName
                                                                       , uDtoLastName  = uLastName
                                                                       }

signupDtoToUserData :: SignupDto -> NewUserData
signupDtoToUserData SignupDto { sDtoUsername, sDtoPassword, sDtoFirstName, sDtoLastName } =
  NewUserData { nudUsername  = sDtoUsername
              , nudPassword  = sDtoPassword
              , nudFirstName = sDtoFirstName
              , nudLastName  = sDtoLastName
              }
