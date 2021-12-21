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
import           Domain.Password                          ( Password )
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
  { ldName     :: !Text
  , ldPassword :: !Password
  }
  deriving Generic

instance FromJSON LoginDto where
  parseJSON = genericParseJSON $ jsonOptions "ld"

data SignupDto = SignupDto
  { sdUsername  :: !Text
  , sdPassword  :: !Password
  , sdFirstName :: !(Maybe Text)
  , sdLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Validity SignupDto where
  validate SignupDto { sdUsername, sdFirstName, sdLastName } = mconcat
    [ declare "Username is at least 5 characters long"    (length sdUsername > 5)
    , declare "Username is not longer than 25 characters" (length sdUsername <= 25)
    , declare "First name is at least 2 characters long"  (maybe True ((> 2) . length) sdFirstName)
    , declare "First name is not longer than 30 characters"
              (maybe True ((<= 30) . length) sdFirstName)
    , declare "Last name is at least 2 characters long" (maybe True ((> 2) . length) sdLastName)
    , declare "Last name is not longer than 30 characters"
              (maybe True ((<= 30) . length) sdLastName)
    ]

instance FromJSON SignupDto where
  parseJSON v = parseJSONValid $ genericParseJSON (jsonOptions "sd") v
