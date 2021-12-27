module Domain.Policy.AccessPolicy
  ( AccessPolicy(..)
  ) where

import           Data.Kind                                          ( Type )
import           Domain.Auth.Permission                             ( Permission )
import           Domain.Auth.UserClaims                             ( UserClaims )
import           RIO                                                ( ($)
                                                                    , Monad
                                                                    , return
                                                                    )


class AccessPolicy entity where
  data Action entity :: Type
  checkAccessPolicy :: UserClaims -> Action entity -> Permission
  checkAccessPolicyM :: (Monad m) => UserClaims -> Action entity -> m Permission
  checkAccessPolicyM claims action = return $ checkAccessPolicy claims action
