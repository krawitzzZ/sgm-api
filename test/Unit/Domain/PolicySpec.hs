module Unit.Domain.PolicySpec
  ( spec
  ) where

import           Data.UUID                                          ( nil )
import           Domain.Auth.Role                                   ( Role(Participant) )
import           Domain.Auth.UserClaims                             ( UserClaims(UserClaims) )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.Policy                                      ( accessPolicyGuard )
import           Domain.User                                        ( Action(DeleteUser) )
import           RIO                                                ( ($)
                                                                    , (==)
                                                                    , IO
                                                                    )
import           Test.Hspec                                         ( Spec
                                                                    , describe
                                                                    , it
                                                                    , parallel
                                                                    , shouldReturn
                                                                    , shouldThrow
                                                                    )
import           TestUtils                                          ( mkUUID )


spec :: Spec
spec = parallel $ do
  describe "accessPolicyGuard" $ do
    it "should throw if user claims do not satisfy action policy" $ do
      accessPolicyGuardThrows `shouldThrow` (== AccessPolicyViolation)
    it "should not throw if user claims do not satisfy action policy (same UserId)" $ do
      accessPolicyGuardPasses `shouldReturn` ()


accessPolicyGuardThrows :: IO ()
accessPolicyGuardThrows = accessPolicyGuard claims action
 where
  claims = UserClaims (mkUUID "c2cc10e1-57d6-4b6f-9899-38d972112d8c") [Participant]
  action = DeleteUser nil

accessPolicyGuardPasses :: IO ()
accessPolicyGuardPasses = accessPolicyGuard claims action
 where
  claims = UserClaims (mkUUID "c2cc10e1-57d6-4b6f-9899-38d972112d8c") [Participant]
  action = DeleteUser (mkUUID "c2cc10e1-57d6-4b6f-9899-38d972112d8c")
