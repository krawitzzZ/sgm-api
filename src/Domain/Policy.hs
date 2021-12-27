module Domain.Policy
  ( accessPolicyGuard
  ) where

import           Control.Exception.Safe                             ( MonadThrow
                                                                    , throwM
                                                                    )
import           Domain.Auth.Permission                             ( Permission(..) )
import           Domain.Auth.UserClaims                             ( UserClaims )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.Policy.AccessPolicy                         ( AccessPolicy(..)
                                                                    , Action
                                                                    )
import           RIO                                                ( (>>=)
                                                                    , return
                                                                    )


accessPolicyGuard :: (AccessPolicy e, MonadThrow m) => UserClaims -> Action e -> m ()
accessPolicyGuard claims entityAction = checkAccessPolicyM claims entityAction >>= \case
  Granted -> return ()
  Denied  -> throwM AccessPolicyViolation
