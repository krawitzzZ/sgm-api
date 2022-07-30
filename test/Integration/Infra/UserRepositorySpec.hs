module Integration.Infra.UserRepositorySpec
  ( spec
  ) where

import           Assertion                                          ( shouldSatisfy' )
import           Control.Exception.Safe                             ( MonadMask
                                                                    , bracket
                                                                    , bracket_
                                                                    )
import           Data.ByteString                                    ( ByteString )
import           Data.List                                          ( head )
import           Data.UUID                                          ( nil
                                                                    , toText
                                                                    )
import           Database.Beam                                      ( (==.)
                                                                    , currentTimestamp_
                                                                    , delete
                                                                    , insert
                                                                    , insertExpressions
                                                                    , runDelete
                                                                    , runInsert
                                                                    , val_
                                                                    )
import           Database.Beam.Postgres                             ( close
                                                                    , connectPostgreSQL
                                                                    )
import           Domain.App.Types                                   ( UserId(..) )
import           Domain.Auth.Password                               ( Password(..) )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.User                                        ( User(..) )
import           Domain.User.UserData                               ( NewUserData(..) )
import           Infra.Beam.Query                                   ( runBeam
                                                                    , usersTable
                                                                    )
import           Infra.Beam.Schema                                  ( migrateSgmDb )
import           Infra.Beam.Schema.Latest                           ( UserEntityT(..) )
import           Infra.UserRepository                               ( createOne
                                                                    , deleteOne
                                                                    , findOneById
                                                                    , findOneByUsername
                                                                    , getAll
                                                                    , saveOne
                                                                    )
import           RIO                                                ( ($)
                                                                    , (<$>)
                                                                    , (<>)
                                                                    , (==)
                                                                    , (>>=)
                                                                    , IO
                                                                    , Maybe(..)
                                                                    , MonadIO
                                                                    , atomically
                                                                    , map
                                                                    , newTMVarIO
                                                                    , putTMVar
                                                                    , takeTMVar
                                                                    , void
                                                                    )
import           RIO.Vector                                         ( fromList )
import           Test.Hspec                                         ( Spec
                                                                    , around
                                                                    , describe
                                                                    , it
                                                                    , shouldBe
                                                                    , shouldReturn
                                                                    , shouldThrow
                                                                    )
import           TestConstants                                      ( uuid1 )
import           TestEnv                                            ( TestEnv(..)
                                                                    , testConfig
                                                                    , testDbUrl
                                                                    )
import           TestUtils                                          ( mkUser
                                                                    , mkUsers
                                                                    )


spec :: Spec
spec = around (withTestEnv testDbUrl) $ do
  describe "GetAll" $ do
    it "should return empty list" $ \e -> do
      withUsers e [] $ getAll e `shouldReturn` []

    it "should return list of users" $ \e -> do
      users <- mkUsers 5
      withUsers e users $ getAll e `shouldReturn` users

  describe "findOneById" $ do
    it "should return a user" $ \e -> do
      users <- mkUsers 3
      withUsers e users $ findOneById e (uId $ head users) `shouldReturn` head users

    it "should throw NotFound if user does not exist" $ \e -> do
      users <- mkUsers 3
      withUsers e users
        $             findOneById e (UserId nil)
        `shouldThrow` (== (NotFound $ "User with id '" <> toText nil <> "' not found"))

  describe "findOneByUsername" $ do
    it "should return a user" $ \e -> do
      users <- mkUsers 3
      withUsers e users $ findOneByUsername e (uUsername $ head users) `shouldReturn` head users

    it "should throw NotFound if user does not exist" $ \e -> do
      users <- mkUsers 3
      withUsers e users
        $             findOneByUsername e "I am a username"
        `shouldThrow` (== NotFound "User with username 'I am a username' not found")

  describe "createOne" $ do
    it "should create a user" $ \e -> do
      let udata = NewUserData { nudUsername  = "username"
                              , nudPassword  = Password "password"
                              , nudRoles     = [Participant]
                              , nudFirstName = Just "first"
                              , nudLastName  = Just "last"
                              }
      withUsers e [] $ createOne e udata `shouldSatisfy'` (\u -> uUsername u == nudUsername udata)

    it "should throw UserNameAlreadyExists if user with the same username already exists" $ \e -> do
      let user = mkUser (UserId uuid1) [Admin]
      let udata = NewUserData { nudUsername  = uUsername user
                              , nudPassword  = Password "password"
                              , nudRoles     = [Participant]
                              , nudFirstName = Just "first"
                              , nudLastName  = Just "last"
                              }
      withUsers e [user]
        $             createOne e udata
        `shouldThrow` (== (  UserNameAlreadyExists
                          $  "User with username '"
                          <> uUsername user
                          <> "' already exists"
                          )
                      )

  describe "saveOne" $ do
    it "should update a user" $ \e -> do
      let user = mkUser (UserId uuid1) [Admin]
      let updatedUser = user { uUsername  = "updated username"
                             , uFirstName = Just "updated first name"
                             , uLastName  = Just "updated last name"
                             , uRoles     = [Superadmin, Admin, Moderator, Participant]
                             }
      withUsers e [user] $ do
        void $ saveOne e updatedUser
        savedUser <- findOneById e (uId user)

        uUsername savedUser `shouldBe` uUsername savedUser
        uFirstName savedUser `shouldBe` uFirstName savedUser
        uLastName savedUser `shouldBe` uLastName savedUser
        uRoles savedUser `shouldBe` uRoles savedUser

  describe "deleteOne" $ do
    it "should delete a user" $ \e -> do
      [user] <- mkUsers 1
      let uid = uId user
      withUsers e [user] $ do
        deleteOne e uid `shouldReturn` ()
        findOneById e uid
          `shouldThrow` (== (NotFound $ "User with id '" <> toText (unUserId uid) <> "' not found"))

    it "should throw NotFound if user does not exist" $ \e -> do
      [user] <- mkUsers 1
      withUsers e [user]
        $             deleteOne e (UserId nil)
        `shouldThrow` (== (NotFound $ "User with id '" <> toText nil <> "' not found"))


withTestEnv :: ByteString -> (TestEnv -> IO a) -> IO a
withTestEnv s = bracket connectAndMakeTestEnv disconnect
 where
  connectAndMakeTestEnv = connectPostgreSQL s >>= \connection -> do
    migrateSgmDb connection
    TestEnv testConfig connection <$> newTMVarIO ()

  disconnect e@TestEnv {..} = do
    runBeam e $ runDelete $ delete usersTable (\UserEntity {..} -> ueId ==. ueId)
    close teDbConn


withUsers :: (MonadIO m, MonadMask m) => TestEnv -> [User] -> m a -> m a
withUsers e@TestEnv {..} users = bracket_ lock unlock
 where
  lock = do
    atomically $ takeTMVar teLock
    saveUsers

  unlock = do
    runBeam e $ runDelete $ delete usersTable (\UserEntity {..} -> ueId ==. ueId)
    atomically $ putTMVar teLock ()

  saveUsers = runBeam e $ do
    runInsert $ insert usersTable $ insertExpressions (map userToEntity users)

  userToEntity User {..} = UserEntity { ueId             = val_ $ unUserId uId
                                      , ueCreatedAt      = currentTimestamp_
                                      , ueLastUpdatedAt  = currentTimestamp_
                                      , ueUsername       = val_ uUsername
                                      , ueProfilePicture = val_ Nothing
                                      , uePassword       = val_ uPassword
                                      , ueRoles          = val_ (fromList uRoles)
                                      , ueFirstName      = val_ uFirstName
                                      , ueLastName       = val_ uLastName
                                      }
