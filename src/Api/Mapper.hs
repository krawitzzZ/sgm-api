module Api.Mapper
  ( userToUserDto
  , createUserDtoToUserData
  ) where

import           Api.Resources.User                       ( CreateUserDto(..)
                                                          , UserDto(..)
                                                          )
import           Domain.User                              ( User(..)
                                                          , UserData(..)
                                                          )


userToUserDto :: User -> UserDto
userToUserDto (User id name _ fname lname) = UserDto id name fname lname

createUserDtoToUserData :: CreateUserDto -> UserData
createUserDtoToUserData (CreateUserDto name pass fname lname) = UserData name pass fname lname
