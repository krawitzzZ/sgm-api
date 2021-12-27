module Domain.Auth.Permission
  ( Permission(..)
  , check
  ) where

import           RIO                                                ( Bool
                                                                    , Monoid(..)
                                                                    , Semigroup(..)
                                                                    )


data Permission = Granted | Denied

instance Semigroup Permission where
  Denied <> Denied = Denied
  _      <> _      = Granted

instance Monoid Permission where
  mempty = Denied

check :: Bool -> Permission
check granted = if granted then Granted else Denied
