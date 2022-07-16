module Unit.Domain.UserSpec
  ( spec
  ) where

import           Domain.App.Types                                   ( UserId(..) )
import           Domain.Auth.Permission                             ( Permission(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Policy                                      ( actionPermission )
import           Domain.User                                        ( Action(..) )
import           RIO                                                ( ($) )
import           Test.Hspec                                         ( Spec
                                                                    , context
                                                                    , describe
                                                                    , it
                                                                    , parallel
                                                                    , shouldBe
                                                                    )
import           Test.Hspec.QuickCheck                              ( prop )
import           TestConstants                                      ( adminClaims
                                                                    , moderatorClaims
                                                                    , participantClaims
                                                                    , superadminClaims
                                                                    )


spec :: Spec
spec = parallel $ do
  describe "AccessPolicy instance" $ do

    context "when user is Superadmin" $ do
      it "should be able to create new users" $ do
        createUser superadminClaims `shouldBe` Granted
      prop "should be able to update any user"
        $ \uid -> updateUser superadminClaims (UserId uid) `shouldBe` Granted
      prop "should be able to delete any user"
        $ \uid -> deleteUser superadminClaims (UserId uid) `shouldBe` Granted
      it "should be able to get all users" $ do
        getUsers superadminClaims `shouldBe` Granted
      it "should be able to get a user" $ do
        getUser superadminClaims `shouldBe` Granted

    context "when user is Admin" $ do
      it "should not be able to create new users" $ do
        createUser adminClaims `shouldBe` Denied
      prop "should be able to update any user"
        $ \uid -> updateUser adminClaims (UserId uid) `shouldBe` Granted
      prop "should not be able to delete any user"
        $ \uid -> deleteUser adminClaims (UserId uid) `shouldBe` Denied
      it "should only be able to delete himself" $ do
        deleteUser adminClaims (ucId adminClaims) `shouldBe` Granted
      it "should be able to get all users" $ do
        getUsers adminClaims `shouldBe` Granted
      it "should be able to get a user" $ do
        getUser adminClaims `shouldBe` Granted

    context "when user is Moderator" $ do
      it "should not be able to create new users" $ do
        createUser moderatorClaims `shouldBe` Denied
      prop "should not be able to update any user"
        $ \uid -> updateUser moderatorClaims (UserId uid) `shouldBe` Denied
      it "should only be able to update himself" $ do
        updateUser moderatorClaims (ucId moderatorClaims) `shouldBe` Granted
      prop "should not be able to delete any user"
        $ \uid -> deleteUser moderatorClaims (UserId uid) `shouldBe` Denied
      it "should only be able to delete himself" $ do
        deleteUser moderatorClaims (ucId moderatorClaims) `shouldBe` Granted
      it "should be able to get all users" $ do
        getUsers moderatorClaims `shouldBe` Granted
      it "should be able to get a user" $ do
        getUser moderatorClaims `shouldBe` Granted

    context "when user is Participant" $ do
      it "should not be able to create new users" $ do
        createUser participantClaims `shouldBe` Denied
      prop "should not be able to update any user"
        $ \uid -> updateUser participantClaims (UserId uid) `shouldBe` Denied
      it "should only be able to update himself" $ do
        updateUser participantClaims (ucId participantClaims) `shouldBe` Granted
      prop "should not be able to delete any user"
        $ \uid -> deleteUser participantClaims (UserId uid) `shouldBe` Denied
      it "should only be able to delete himself" $ do
        deleteUser participantClaims (ucId participantClaims) `shouldBe` Granted
      it "should be able to get all users" $ do
        getUsers participantClaims `shouldBe` Granted
      it "should be able to get a user" $ do
        getUser participantClaims `shouldBe` Granted


createUser :: UserClaims -> Permission
createUser c = actionPermission c CreateUser

updateUser :: UserClaims -> UserId -> Permission
updateUser c uid = actionPermission c (UpdateUserInfo uid)

deleteUser :: UserClaims -> UserId -> Permission
deleteUser c uid = actionPermission c (DeleteUser uid)

getUsers :: UserClaims -> Permission
getUsers c = actionPermission c GetAllUsers

getUser :: UserClaims -> Permission
getUser c = actionPermission c GetUser
