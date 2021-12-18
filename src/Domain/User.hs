module Domain.User
  ( User(..)
  , UserData(..)
  ) where

import           Data.UUID                                ( UUID )
import           Domain.Password                          ( Password
                                                          , PasswordHash
                                                          )
import           RIO                                      ( (<>)
                                                          , (==)
                                                          , Eq
                                                          , Generic
                                                          , Maybe
                                                          , Show(..)
                                                          , Text
                                                          , on
                                                          )


data User = User
  { userId        :: !UUID
  , userName      :: !Text
  , userPassword  :: !PasswordHash
  , userFirstName :: !(Maybe Text)
  , userLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Eq User where
  (==) = (==) `on` userId

instance Show User where
  show (User uId name _ fname lname) =
    "User { userId = "
      <> show uId
      <> ", userName = "
      <> show name
      <> ", userFirstName = "
      <> show fname
      <> ", userLastName = "
      <> show lname
      <> " }"

data UserData = UserData
  { userDataName      :: !Text
  , userDataPassword  :: !Password
  , userDataFirstName :: !(Maybe Text)
  , userDataLastName  :: !(Maybe Text)
  }
  deriving Generic

instance Eq UserData where
  (==) = (==) `on` userDataName

instance Show UserData where
  show (UserData name _ fname lname) =
    "UserData { userDataName = "
      <> show name
      <> ", userFirstName = "
      <> show fname
      <> ", userLastName = "
      <> show lname
      <> " }"
