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
userToUserDto User { uId, uUsername, uFirstName, uLastName } =
  UserDto { udId = uId, udUsername = uUsername, udFirstName = uFirstName, udLastName = uLastName }

signupDtoToUserData :: SignupDto -> NewUserData
signupDtoToUserData SignupDto { sdUsername, sdPassword, sdFirstName, sdLastName } = NewUserData
  { nudUsername  = sdUsername
  , nudPassword  = sdPassword
  , nudFirstName = sdFirstName
  , nudLastName  = sdLastName
  }
