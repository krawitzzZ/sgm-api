module Api.Resources.User
  ( UserDto(..)
  , UpdateUserDto(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          , genericParseJSON
                                                          , genericToJSON
                                                          )
import           Data.UUID                                ( UUID )
import           RIO                                      ( ($)
                                                          , Generic
                                                          , Maybe
                                                          , Show
                                                          , Text
                                                          )
import           Utils                                    ( jsonOptions )


data UserDto = UserDto
  { userDtoId        :: !UUID
  , userDtoName      :: !Text
  , userDtoFirstName :: !(Maybe Text)
  , userDtoLastName  :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance ToJSON UserDto where
  toJSON = genericToJSON $ jsonOptions "userDto"

data UpdateUserDto = UpdateUserDto
  { updateUserDtoFirstName :: !(Maybe Text)
  , updateUserDtoLastName  :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON UpdateUserDto where
  parseJSON = genericParseJSON $ jsonOptions "updateUserDto"
