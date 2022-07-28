module Api.Resources.User
  ( UserDto(..)
  , UpdateUserDto(..)
  ) where

import           Data.Aeson                                         ( FromJSON(..)
                                                                    , ToJSON(..)
                                                                    , genericParseJSON
                                                                    , genericToJSON
                                                                    )
import           Data.Validity                                      ( Validity(..)
                                                                    , declare
                                                                    )
import           Data.Validity.Aeson                                ( parseJSONValid )
import           Data.Validity.Text                                 ( )
import           Data.Validity.UUID                                 ( )
import           Domain.App.Types                                   ( UserId )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<=)
                                                                    , (>)
                                                                    , Bool(..)
                                                                    , Eq
                                                                    , Generic
                                                                    , Maybe
                                                                    , Show
                                                                    , Text
                                                                    , maybe
                                                                    , mconcat
                                                                    )
import           RIO.Text                                           ( length )
import           Utils                                              ( jsonOptions )


data UserDto = UserDto
  { uDtoId        :: !UserId
  , uDtoUsername  :: !Text
  , uDtoFirstName :: !(Maybe Text)
  , uDtoLastName  :: !(Maybe Text)
  }
  deriving (Generic, Eq, Show)

instance ToJSON UserDto where
  toJSON = genericToJSON $ jsonOptions "uDto"
instance FromJSON UserDto where
  parseJSON = genericParseJSON $ jsonOptions "uDto"

data UpdateUserDto = UpdateUserDto
  { uuDtoFirstName :: !(Maybe Text)
  , uuDtoLastName  :: !(Maybe Text)
  }
  deriving (Generic, Eq, Show)

instance ToJSON UpdateUserDto where
  toJSON = genericToJSON $ jsonOptions "uuDto"
instance FromJSON UpdateUserDto where
  parseJSON v = parseJSONValid $ genericParseJSON (jsonOptions "uuDto") v

instance Validity UpdateUserDto where
  validate UpdateUserDto {..} = mconcat
    [ declare "First name is at least 2 characters long"
              (maybe True ((> 2) . length) uuDtoFirstName)
    , declare "First name is not longer than 30 characters"
              (maybe True ((<= 30) . length) uuDtoFirstName)
    , declare "Last name is at least 2 characters long" (maybe True ((> 2) . length) uuDtoLastName)
    , declare "Last name is not longer than 30 characters"
              (maybe True ((<= 30) . length) uuDtoLastName)
    ]
