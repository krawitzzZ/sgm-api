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
  { lDtoName     :: !Text
  , lDtoPassword :: !Password
  }
  deriving (Show, Generic)

instance FromJSON LoginDto where
  parseJSON = genericParseJSON $ jsonOptions "lDto"

data SignupDto = SignupDto
  { sDtoUsername  :: !Text
  , sDtoPassword  :: !Password
  , sDtoFirstName :: !(Maybe Text)
  , sDtoLastName  :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON SignupDto where
  parseJSON = genericParseJSON $ jsonOptions "sDto"
