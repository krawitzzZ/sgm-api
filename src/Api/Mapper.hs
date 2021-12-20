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
userToUserDto (User id name _ fname lname) = UserDto id name fname lname

signupDtoToUserData :: SignupDto -> UserData
signupDtoToUserData (SignupDto name pass fname lname) = UserData name pass fname lname
