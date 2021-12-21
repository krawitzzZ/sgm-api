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
import           Data.Validity                            ( Validity(..)
                                                          , declare
                                                          )
import           Data.Validity.Aeson                      ( parseJSONValid )
import           Data.Validity.Text                       ( )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (>)
                                                          , Bool(..)
                                                          , Generic
                                                          , Maybe
                                                          , Text
                                                          , maybe
                                                          , mconcat
                                                          )
import           RIO.Text                                 ( length )
import           Utils                                    ( jsonOptions )


data UserDto = UserDto
  { uDtoId        :: !UUID
  , uDtoUsername  :: !Text
  , uDtoFirstName :: !(Maybe Text)
  , uDtoLastName  :: !(Maybe Text)
  }
  deriving Generic

instance ToJSON UserDto where
  toJSON = genericToJSON $ jsonOptions "uDto"

data UpdateUserDto = UpdateUserDto
  { uuDtoFirstName :: !(Maybe Text)
  , uuDtoLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Validity UpdateUserDto where
  validate UpdateUserDto { uuDtoFirstName, uuDtoLastName } = mconcat
    [ declare "First name is at least 2 characters long"
              (maybe True ((> 2) . length) uuDtoFirstName)
    , declare "Last name is at least 2 characters long" (maybe True ((> 2) . length) uuDtoLastName)
    ]

instance FromJSON UpdateUserDto where
  parseJSON v = parseJSONValid $ genericParseJSON (jsonOptions "uuDto") v
