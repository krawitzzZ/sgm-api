module Api.Mapper
  ( userToUserDto
  , signupDtoToUserData
  ) where

import           Api.Resources.Auth                       ( SignupDto(..) )
import           Api.Resources.User                       ( UserDto(..) )
import           Domain.User                              ( User(..)
                                                          , UserData(..)
                                                          )


userToUserDto :: User -> UserDto
userToUserDto (User id username _ fname lname) = UserDto id username fname lname

signupDtoToUserData :: SignupDto -> UserData
signupDtoToUserData (SignupDto username pass fname lname) = UserData username pass fname lname
