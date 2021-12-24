module Domain.User.UserData
  ( NewUserData(..)
  ) where

import           Domain.Auth.Password                     ( Password )
import           Domain.Auth.Role                         ( Role )
import           RIO                                      ( Maybe
                                                          , Text
                                                          )


data NewUserData = NewUserData
  { nudUsername  :: !Text
  , nudPassword  :: !Password
  , nudRoles     :: ![Role]
  , nudFirstName :: !(Maybe Text)
  , nudLastName  :: !(Maybe Text)
  }
