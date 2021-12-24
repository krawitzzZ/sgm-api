module Domain.Auth.UserClaims
  ( UserClaims(..)
  ) where

import           Control.Monad.Reader.Has                 ( Has )
import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          , genericParseJSON
                                                          , genericToJSON
                                                          )
import           Data.UUID                                ( UUID )
import           Domain.Auth.Role                         ( Role )
import           RIO                                      ( ($)
                                                          , Generic
                                                          )
import           Servant.Auth.JWT                         ( FromJWT
                                                          , ToJWT
                                                          )
import           Utils                                    ( jsonOptions )


data UserClaims = UserClaims
  { ucId    :: !UUID
  , ucRoles :: ![Role]
  }
  deriving (Generic, Has UUID)

instance ToJSON UserClaims where
  toJSON = genericToJSON $ jsonOptions "uc"

instance FromJSON UserClaims where
  parseJSON = genericParseJSON $ jsonOptions "uc"

instance ToJWT UserClaims
instance FromJWT UserClaims
