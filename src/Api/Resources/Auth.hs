module Api.Resources.Auth
  ( SignupDto(..)
  , LoginDto(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , genericParseJSON
                                                          )
import           Domain.Password                          ( Password )
import           RIO                                      ( ($)
                                                          , Generic
                                                          , Maybe
                                                          , Show
                                                          , Text
                                                          )
import           Utils                                    ( jsonOptions )


data LoginDto = LoginDto
  { loginDtoName     :: !Text
  , loginDtoPassword :: !Password
  }
  deriving (Show, Generic)

instance FromJSON LoginDto where
  parseJSON = genericParseJSON $ jsonOptions "loginDto"

data SignupDto = SignupDto
  { signupDtoName      :: !Text
  , signupDtoPassword  :: !Password
  , signupDtoFirstName :: !(Maybe Text)
  , signupDtoLastName  :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON SignupDto where
  parseJSON = genericParseJSON $ jsonOptions "signupDto"
