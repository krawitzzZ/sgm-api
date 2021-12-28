module Domain.User
  ( User(..)
  , Action(..)
  ) where

import           Domain.App.Types                                   ( UserId )
import           Domain.Auth.Password                               ( PasswordHash )
import           Domain.Auth.Permission                             ( Permission(..)
                                                                    , check
                                                                    )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Policy.AccessPolicy                         ( AccessPolicy(..) )
import           RIO                                                ( (<>)
                                                                    , (==)
                                                                    , Eq
                                                                    , Maybe
                                                                    , Semigroup(..)
                                                                    , Text
                                                                    , elem
                                                                    , on
                                                                    )
import           Utils                                              ( anyElem )


data User = User
  { uId        :: !UserId
  , uUsername  :: !Text
  , uPassword  :: !PasswordHash
  , uRoles     :: ![Role]
  , uFirstName :: !(Maybe Text)
  , uLastName  :: !(Maybe Text)
  }

instance Eq User where
  (==) = (==) `on` uId

instance AccessPolicy User where
  data Action User =
    CreateUser |
    GetUser |
    GetAllUsers |
    UpdateUserInfo UserId |
    DeleteUser UserId

  checkAccessPolicy UserClaims {..} CreateUser  = check (Superadmin `elem` ucRoles)
  checkAccessPolicy _               GetUser     = Granted
  checkAccessPolicy _               GetAllUsers = Granted
  checkAccessPolicy UserClaims {..} (UpdateUserInfo userId) =
    check (ucId == userId) <> check ([Admin, Superadmin] `anyElem` ucRoles)
  checkAccessPolicy UserClaims {..} (DeleteUser userId) =
    check (ucId == userId) <> check (Superadmin `elem` ucRoles)
