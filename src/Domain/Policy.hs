module Domain.Policy
  ( HasActionPolicy(..)
  ) where

import           Data.Kind                                          ( Type )
import           Domain.Auth.Permission                             ( Permission )
import           Domain.Auth.UserClaims                             ( UserClaims )

class HasActionPolicy entity where
  data Action entity :: Type
  actionPermission :: UserClaims -> Action entity -> Permission
  permittedActions :: UserClaims -> entity -> [Action entity]
