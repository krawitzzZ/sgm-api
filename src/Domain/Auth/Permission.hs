module Domain.Auth.Permission
  ( Permission(..)
  , check
  , isPermitted
  ) where

import           RIO                                                ( Bool(..)
                                                                    , Eq
                                                                    , Monoid(..)
                                                                    , Semigroup(..)
                                                                    , Show
                                                                    )


data Permission = Granted | Denied deriving (Eq, Show)

instance Semigroup Permission where
  Denied <> Denied = Denied
  _      <> _      = Granted

instance Monoid Permission where
  mempty = Denied

check :: Bool -> Permission
check True  = Granted
check False = Denied

isPermitted :: Permission -> Bool
isPermitted Granted = True
isPermitted Denied  = False
