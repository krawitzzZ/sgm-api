module Domain.User
  ( User(..)
  , UserData(..)
  , Id
  , UserRepository(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          , genericParseJSON
                                                          , genericToJSON
                                                          )
import           Data.UUID                                ( UUID )
import           RIO                                      ( ($)
                                                          , Eq
                                                          , Generic
                                                          , Maybe(..)
                                                          , Show
                                                          , Text
                                                          )
import           Utils                                    ( jsonOptions )


type Id = UUID

data User = User
  { userId        :: !UUID
  , userFirstName :: !Text
  , userLastName  :: !Text
  , userName      :: !Text
  , userPassword  :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON $ jsonOptions "user"

instance ToJSON User where
  toJSON = genericToJSON $ jsonOptions "user"

data UserData = UserData
  { userDataFirstName :: Maybe Text
  , userDataLastName  :: Maybe Text
  , userDataName      :: Maybe Text
  , userDataPassword  :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserData where
  parseJSON = genericParseJSON $ jsonOptions "userData"

instance ToJSON UserData where
  toJSON = genericToJSON $ jsonOptions "userData"

class UserRepository m where
  getUserById :: Id -> m User
  getAllUsers :: m [User]
  createUser :: UserData -> m User
  updateUser :: Id -> UserData -> m User
  deleteUser :: Id -> m ()
