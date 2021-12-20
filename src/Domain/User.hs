module Domain.User
  ( User(..)
  , UserData(..)
  ) where

import           Data.UUID                                ( UUID )
import           Domain.Password                          ( Password
                                                          , PasswordHash
                                                          )
import           RIO                                      ( (==)
                                                          , Eq
                                                          , Generic
                                                          , Maybe
                                                          , Text
                                                          , on
                                                          )


data User = User
  { uId        :: !UUID
  , uUsername  :: !Text
  , uPassword  :: !PasswordHash
  , uFirstName :: !(Maybe Text)
  , uLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Eq User where
  (==) = (==) `on` uId

data UserData = UserData
  { udUsername  :: !Text
  , udPassword  :: !Password
  , udFirstName :: !(Maybe Text)
  , udLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Eq UserData where
  (==) = (==) `on` udUsername
