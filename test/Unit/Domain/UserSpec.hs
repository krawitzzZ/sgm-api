module Unit.Domain.UserSpec
  ( spec
  ) where

import           Data.UUID                                          ( UUID
                                                                    , nil
                                                                    )
import           Domain.Auth.Permission                             ( Permission(..) )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Policy.AccessPolicy                         ( checkAccessPolicy )
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


spec :: Spec
spec = parallel $ do
  describe "AccessPolicy instance" $ do

    context "when user is Superadmin" $ do
      it "should be able to create new users" $ do
        createUser superadminClaims `shouldBe` Granted
      prop "should be able to update any user"
        $ \uid -> updateUser superadminClaims uid `shouldBe` Granted
      prop "should be able to delete any user"
        $ \uid -> deleteUser superadminClaims uid `shouldBe` Granted
      it "should be able to get all users" $ do
        getUsers superadminClaims `shouldBe` Granted
      it "should be able to get a user" $ do
        getUser superadminClaims `shouldBe` Granted

    context "when user is Admin" $ do
      it "should not be able to create new users" $ do
        createUser adminClaims `shouldBe` Denied
      prop "should be able to update any user"
        $ \uid -> updateUser adminClaims uid `shouldBe` Granted
      prop "should not be able to delete any user"
        $ \uid -> deleteUser adminClaims uid `shouldBe` Denied
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
        $ \uid -> updateUser moderatorClaims uid `shouldBe` Denied
      it "should only be able to update himself" $ do
        updateUser moderatorClaims (ucId moderatorClaims) `shouldBe` Granted
      prop "should not be able to delete any user"
        $ \uid -> deleteUser moderatorClaims uid `shouldBe` Denied
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
        $ \uid -> updateUser participantClaims uid `shouldBe` Denied
      it "should only be able to update himself" $ do
        updateUser participantClaims (ucId participantClaims) `shouldBe` Granted
      prop "should not be able to delete any user"
        $ \uid -> deleteUser participantClaims uid `shouldBe` Denied
      it "should only be able to delete himself" $ do
        deleteUser participantClaims (ucId participantClaims) `shouldBe` Granted
      it "should be able to get all users" $ do
        getUsers participantClaims `shouldBe` Granted
      it "should be able to get a user" $ do
        getUser participantClaims `shouldBe` Granted


createUser :: UserClaims -> Permission
createUser c = checkAccessPolicy c CreateUser

updateUser :: UserClaims -> UUID -> Permission
updateUser c uid = checkAccessPolicy c (UpdateUserInfo uid)

deleteUser :: UserClaims -> UUID -> Permission
deleteUser c uid = checkAccessPolicy c (DeleteUser uid)

getUsers :: UserClaims -> Permission
getUsers c = checkAccessPolicy c GetAllUsers

getUser :: UserClaims -> Permission
getUser c = checkAccessPolicy c GetUser

superadminClaims :: UserClaims
superadminClaims = UserClaims nil [Superadmin]

adminClaims :: UserClaims
adminClaims = UserClaims nil [Admin]

moderatorClaims :: UserClaims
moderatorClaims = UserClaims nil [Moderator]

participantClaims :: UserClaims
participantClaims = UserClaims nil [Participant]
