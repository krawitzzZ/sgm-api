module Domain.User
  ( User(..)
  , Action(..)
  ) where

import           Domain.App.Types                                   ( UserId )
import           Domain.Auth.Password                               ( PasswordHash )
import           Domain.Auth.Permission                             ( Permission(..)
                                                                    , check
                                                                    , isPermitted
                                                                    )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Policy                                      ( HasActionPolicy(..) )
import           RIO                                                ( (.)
                                                                    , (<>)
                                                                    , (==)
                                                                    , Eq
                                                                    , Maybe
                                                                    , Semigroup(..)
                                                                    , Show
                                                                    , Text
                                                                    , elem
                                                                    , filter
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
  deriving Show

instance Eq User where
  (==) = (==) `on` uId

instance HasActionPolicy User where
  data Action User =
    CreateUser |
    GetUser |
    GetAllUsers |
    UpdateUserInfo UserId |
    DeleteUser UserId
    deriving (Eq, Show)

  actionPermission UserClaims {..} CreateUser  = check (Superadmin `elem` ucRoles)
  actionPermission _               GetUser     = Granted
  actionPermission _               GetAllUsers = Granted
  actionPermission UserClaims {..} (UpdateUserInfo userId) =
    check (ucId == userId) <> check ([Admin, Superadmin] `anyElem` ucRoles)
  actionPermission UserClaims {..} (DeleteUser userId) =
    check (ucId == userId) <> check (Superadmin `elem` ucRoles)

  permittedActions uc User {..} = filter (permitted uc) allActions
   where
    permitted c = isPermitted . actionPermission c
    allActions = [CreateUser, GetUser, GetAllUsers, UpdateUserInfo uId, DeleteUser uId]
