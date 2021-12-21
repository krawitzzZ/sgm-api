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
  { uDtoId        :: !UUID
  , uDtoUsername  :: !Text
  , uDtoFirstName :: !(Maybe Text)
  , uDtoLastName  :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance ToJSON UserDto where
  toJSON = genericToJSON $ jsonOptions "uDto"

data UpdateUserDto = UpdateUserDto
  { uuDtoFirstName :: !(Maybe Text)
  , uuDtoLastName  :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON UpdateUserDto where
  parseJSON = genericParseJSON $ jsonOptions "uuDto"
