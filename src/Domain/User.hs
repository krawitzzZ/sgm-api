module Domain.User
  ( User(..)
  , Id
  , UserRepository(..)
  , HasUserRepository(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          , genericParseJSON
                                                          , genericToJSON
                                                          )
import           RIO                                      ( ($)
                                                          , Eq
                                                          , Generic
                                                          , IO
                                                          , Show
                                                          , Text
                                                          , id
                                                          )
import           Utils                                    ( jsonOptions )


type Id = Text -- TODO: use UUID

data User = User
  { userId        :: Id
  , userFirstName :: Text
  , userLastName  :: Text
  , userEmail     :: Text
  , userPassword  :: Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON User where
  parseJSON = genericParseJSON $ jsonOptions "user"
instance ToJSON User where
  toJSON = genericToJSON $ jsonOptions "user"

data UserRepository = UserRepository
  { findOne   :: !(Id ->  IO User)
  , get       :: !(IO [User])
  , saveOne   :: !(User ->  IO ())
  , upsertOne :: !(User ->  IO ())
  , deleteOne :: !(Id ->  IO ())
  }
class HasUserRepository env where
  getUserRepository :: env -> UserRepository
instance HasUserRepository UserRepository where
  getUserRepository = id
