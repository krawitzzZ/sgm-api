module Domain.User
  ( User(..)
  , NewUserData(..)
  ) where

import           Data.UUID                                ( UUID )
import           Domain.Password                          ( Password
                                                          , PasswordHash
                                                          )
import           Domain.Role                              ( Role )
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
  , uRoles     :: ![Role]
  , uFirstName :: !(Maybe Text)
  , uLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Eq User where
  (==) = (==) `on` uId

data NewUserData = NewUserData
  { nudUsername  :: !Text
  , nudPassword  :: !Password
  , nudRoles     :: ![Role]
  , nudFirstName :: !(Maybe Text)
  , nudLastName  :: !(Maybe Text)
  }
  deriving Generic
