module Domain.Role
  ( Role(..)
  ) where

import           Data.Aeson                               ( FromJSON
                                                          , ToJSON
                                                          )
import           RIO                                      ( Bounded
                                                          , Enum
                                                          , Eq
                                                          , Generic
                                                          , Ord
                                                          , Read
                                                          , Show
                                                          )


data Role =
  Participant |
  Moderator |
  Admin |
  Superadmin
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Generic)

instance ToJSON Role
instance FromJSON Role where
