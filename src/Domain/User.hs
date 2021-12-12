module Domain.User
  ( User(..)
  , UserData(..)
  , Id
  , UserRepository(..)
  ) where

import           Control.Exception.Safe                   ( MonadThrow )
import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          , genericParseJSON
                                                          , genericToJSON
                                                          )
import           RIO                                      ( ($)
                                                          , Eq
                                                          , Generic
                                                          , Maybe(..)
                                                          , Show
                                                          , Text
                                                          )
import           Utils                                    ( jsonOptions )


type Id = Text -- TODO: use UUID

data User = User
  { userId        :: !Id
  , userFirstName :: !Text
  , userLastName  :: !Text
  , userEmail     :: !Text
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
  , userDataEmail     :: Maybe Text
  , userDataPassword  :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserData where
  parseJSON = genericParseJSON $ jsonOptions "userData"

instance ToJSON UserData where
  toJSON = genericToJSON $ jsonOptions "userData"

class MonadThrow m => UserRepository m where
  getUserById :: Id -> m User
  getAllUsers :: m [User]
  createUser :: UserData -> m User
  updateUser :: Id -> UserData -> m User
  deleteUser :: Id -> m ()
