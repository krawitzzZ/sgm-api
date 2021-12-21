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
                                                          , (<=)
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
  { udId        :: !UUID
  , udUsername  :: !Text
  , udFirstName :: !(Maybe Text)
  , udLastName  :: !(Maybe Text)
  }
  deriving Generic

instance ToJSON UserDto where
  toJSON = genericToJSON $ jsonOptions "ud"

data UpdateUserDto = UpdateUserDto
  { uudFirstName :: !(Maybe Text)
  , uudLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Validity UpdateUserDto where
  validate UpdateUserDto { uudFirstName, uudLastName } = mconcat
    [ declare "First name is at least 2 characters long" (maybe True ((> 2) . length) uudFirstName)
    , declare "First name is not longer than 30 characters"
              (maybe True ((<= 30) . length) uudFirstName)
    , declare "Last name is at least 2 characters long" (maybe True ((> 2) . length) uudLastName)
    , declare "Last name is not longer than 30 characters"
              (maybe True ((<= 30) . length) uudLastName)
    ]

instance FromJSON UpdateUserDto where
  parseJSON v = parseJSONValid $ genericParseJSON (jsonOptions "uud") v
