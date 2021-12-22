module Api.Resources.Auth
  ( SignupDto(..)
  , LoginDto(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , genericParseJSON
                                                          )
import           Data.Validity                            ( Validity(..)
                                                          , declare
                                                          )
import           Data.Validity.Aeson                      ( parseJSONValid )
import           Data.Validity.Text                       ( )
import           Domain.Auth.Password                          ( Password )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<=)
                                                          , (>)
                                                          , Bool(..)
                                                          , Generic
                                                          , Maybe(..)
                                                          , Text
                                                          , maybe
                                                          , mconcat
                                                          )
import           RIO.Text                                 ( length )
import           Utils                                    ( jsonOptions )


data LoginDto = LoginDto
  { lDtoName     :: !Text
  , lDtoPassword :: !Password
  }
  deriving Generic

instance FromJSON LoginDto where
  parseJSON = genericParseJSON $ jsonOptions "lDto"

data SignupDto = SignupDto
  { sDtoUsername  :: !Text
  , sDtoPassword  :: !Password
  , sDtoFirstName :: !(Maybe Text)
  , sDtoLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Validity SignupDto where
  validate SignupDto { sDtoUsername, sDtoFirstName, sDtoLastName } = mconcat
    [ declare "Username is at least 5 characters long"    (length sDtoUsername > 5)
    , declare "Username is not longer than 25 characters" (length sDtoUsername <= 25)
    , declare "First name is at least 2 characters long" (maybe True ((> 2) . length) sDtoFirstName)
    , declare "First name is not longer than 30 characters"
              (maybe True ((<= 30) . length) sDtoFirstName)
    , declare "Last name is at least 2 characters long" (maybe True ((> 2) . length) sDtoLastName)
    , declare "Last name is not longer than 30 characters"
              (maybe True ((<= 30) . length) sDtoLastName)
    ]

instance FromJSON SignupDto where
  parseJSON v = parseJSONValid $ genericParseJSON (jsonOptions "sDto") v
