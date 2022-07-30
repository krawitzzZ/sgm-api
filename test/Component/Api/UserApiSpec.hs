module Component.Api.UserApiSpec
  ( spec
  ) where

import           Api.ApiVersion                                     ( ApiVersion(..) )
import           Api.Mapper                                         ( userToUserDto )
import           Api.Resources.User                                 ( UpdateUserDto(..)
                                                                    , UserDto(..)
                                                                    )
import           Api.UserApi                                        ( UserApi
                                                                    , userServer
                                                                    )
import           Assertion                                          ( shouldFailWithStatus )
import           Control.Exception.Safe                             ( MonadCatch
                                                                    , MonadMask
                                                                    , MonadThrow
                                                                    , bracket_
                                                                    )
import qualified Domain.App.Class                                  as C
import           Domain.App.Types                                   ( UserId(..) )
import           Domain.Auth.Role                                   ( Role(..) )
import           Domain.User                                        ( User(..) )
import qualified Infra.InMemory.Query.User                         as UR
import           Network.HTTP.Client                                ( defaultManagerSettings
                                                                    , newManager
                                                                    )
import           Network.HTTP.Types                                 ( status401
                                                                    , status404
                                                                    )
import           Network.Wai.Handler.Warp                           ( Port
                                                                    , testWithApplication
                                                                    )
import           Orphans                                            ( )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<$>)
                                                                    , (<&>)
                                                                    , (<*>)
                                                                    , (>>=)
                                                                    , Applicative
                                                                    , Either(..)
                                                                    , Functor
                                                                    , Generic
                                                                    , Int
                                                                    , Maybe(..)
                                                                    , Monad
                                                                    , MonadIO
                                                                    , MonadReader
                                                                    , MonadTrans(..)
                                                                    , Proxy(..)
                                                                    , ReaderT(..)
                                                                    , ask
                                                                    , atomically
                                                                    , const
                                                                    , flip
                                                                    , map
                                                                    , newTMVarIO
                                                                    , newTVarIO
                                                                    , putTMVar
                                                                    , return
                                                                    , takeTMVar
                                                                    , writeTVar
                                                                    )
import           Servant                                            ( type (:<|>)(..)
                                                                    , NoContent(..)
                                                                    )
import           Servant.Auth                                       ( JWT )
import           Servant.Auth.Client                                ( Bearer )
import           Servant.Auth.Server                                ( JWTSettings
                                                                    , defaultCookieSettings
                                                                    , defaultJWTSettings
                                                                    , generateKey
                                                                    )
import           Servant.Client                                     ( ClientEnv
                                                                    , client
                                                                    , mkClientEnv
                                                                    , parseBaseUrl
                                                                    , runClientM
                                                                    )
import           Servant.QuickCheck                                 ( BaseUrl(..) )
import           Servant.Server                                     ( Application
                                                                    , Context(..)
                                                                    , hoistServerWithContext
                                                                    , serveWithContext
                                                                    )
import           Server                                             ( contextApi
                                                                    , customFormatters
                                                                    )
import           Test.Hspec                                         ( Spec
                                                                    , SpecWith
                                                                    , around
                                                                    , describe
                                                                    , it
                                                                    , parallel
                                                                    , runIO
                                                                    , shouldReturn
                                                                    )
import           TestConstants                                      ( superadminClaims
                                                                    , uuid1
                                                                    , uuid2
                                                                    , uuid3
                                                                    )
import           TestUtils                                          ( mkJwtToken
                                                                    , mkUser
                                                                    )


spec :: Spec
spec = parallel $ do
  let userApiProxy = Proxy :: Proxy (UserApi '[Bearer])
  jwtConf <- runIO $ generateKey <&> defaultJWTSettings
  token   <- runIO $ mkJwtToken superadminClaims jwtConf
  imu     <- runIO $ UR.InMemoryUsers <$> newTVarIO [] <*> newTMVarIO ()
  let uid1  = UserId uuid1
  let uid2  = UserId uuid2
  let uid3  = UserId uuid3
  let user1 = mkUser uid1 [Superadmin]
  let user2 = mkUser uid2 [Admin]
  let user3 = mkUser uid3 [Moderator]

  withUserApp jwtConf imu $ \mkEnv -> do
    let (getAll :<|> getOne :<|> updateOne :<|> deleteOne) = client userApiProxy token
    let (getAll401 :<|> getOne401 :<|> updateOne401 :<|> deleteOne401) =
          client userApiProxy "invalid_token"

    describe "GetUsers" $ do
      it "should return list of users" $ \port -> do
        withUsers imu [user2] $ do
          runClientM getAll (mkEnv port) `shouldReturn` Right (map userToUserDto [user2])

      it "should return 401 if auth token is invalid" $ \port -> do
        withUsers imu [] $ runClientM getAll401 (mkEnv port) `shouldFailWithStatus` status401

    describe "GetUser" $ do
      it "should return a user" $ \port -> do
        withUsers imu [user1] $ do
          runClientM (getOne uid1) (mkEnv port) `shouldReturn` Right (userToUserDto user1)

      it "should return 401 if auth token is invalid" $ \port -> do
        withUsers imu [] $ do
          runClientM (getOne401 uid1) (mkEnv port) `shouldFailWithStatus` status401

      it "should return 404 if user does not exist" $ \port -> do
        withUsers imu [] $ do
          runClientM (getOne uid1) (mkEnv port) `shouldFailWithStatus` status404

    describe "UpdateUser" $ do
      let uuDto = UpdateUserDto (Just "first name") (Just "last name")

      it "should update a user" $ \port -> do
        let expected = (userToUserDto user3) { uDtoFirstName = uuDtoFirstName uuDto
                                             , uDtoLastName  = uuDtoLastName uuDto
                                             }
        withUsers imu [user3] $ do
          runClientM (updateOne uid3 uuDto) (mkEnv port) `shouldReturn` Right expected

      it "should return 401 if auth token is invalid" $ \port -> do
        withUsers imu [] $ do
          runClientM (updateOne401 uid1 uuDto) (mkEnv port) `shouldFailWithStatus` status401

      it "should return 404 if user does not exist" $ \port -> do
        withUsers imu [user1] $ do
          runClientM (updateOne uid3 uuDto) (mkEnv port) `shouldFailWithStatus` status404

    describe "DeleteUser" $ do
      it "should delete a user" $ \port -> do
        withUsers imu [user2] $ do
          runClientM (deleteOne uid2) (mkEnv port) `shouldReturn` Right NoContent

      it "should return 401 if auth token is invalid" $ \port -> do
        withUsers imu [] $ do
          runClientM (deleteOne401 uid3) (mkEnv port) `shouldFailWithStatus` status401

      it "should return 404 if user does not exist" $ \port -> do
        withUsers imu [] $ do
          runClientM (deleteOne uid1) (mkEnv port) `shouldFailWithStatus` status404


userApp :: JWTSettings -> UR.InMemoryUsers -> ApiVersion -> Application
userApp jwtConf us v = serveWithContext userServerApi (context jwtConf)
  $ hoistServer (userServer v)
 where
  userServerApi = Proxy :: Proxy (UserApi '[JWT])
  cookieConf    = defaultCookieSettings
  context jwt = customFormatters :. cookieConf :. jwt :. EmptyContext
  hoistServer = hoistServerWithContext userServerApi contextApi (runUserApp us)

newtype UserApp m a = UserApp { unUserApp :: ReaderT UR.InMemoryUsers m a }
  deriving (Functor, Generic)
  deriving newtype ( Applicative
                   , Monad
                   , MonadIO
                   , MonadThrow
                   , MonadCatch
                   , MonadReader UR.InMemoryUsers
                   )

withUserApp :: JWTSettings -> UR.InMemoryUsers -> ((Int -> ClientEnv) -> SpecWith Port) -> Spec
withUserApp jwtConf users testSuite = do
  manager <- runIO $ newManager defaultManagerSettings
  baseUrl <- runIO $ parseBaseUrl "http://localhost"
  let app = return $ userApp jwtConf users V1
  let mkClientEnv' port = mkClientEnv manager (baseUrl { baseUrlPort = port })

  around (testWithApplication app) (testSuite mkClientEnv')

withUsers :: (MonadIO m, MonadMask m) => UR.InMemoryUsers -> [User] -> m a -> m a
withUsers UR.InMemoryUsers {..} users = bracket_ lock release
 where
  lock = atomically $ do
    takeTMVar imLock
    writeTVar imUsers users
  release = atomically $ do
    writeTVar imUsers []
    putTMVar imLock ()

runUserApp :: UR.InMemoryUsers -> UserApp m a -> m a
runUserApp imus ua = runReaderT (unUserApp ua) imus

instance MonadTrans UserApp where
  lift = UserApp . lift

instance (Monad m) => C.MonadLogger (UserApp m) where
  logDebug = const $ return ()
  logInfo  = const $ return ()
  logWarn  = const $ return ()
  logError = const $ return ()
  withContext _ a = a
  withError _ a = a
  withField _ a = a
  withFields _ a = a

instance (MonadThrow m) => C.AccessPolicyGuard (UserApp m)

instance (MonadIO m, MonadCatch m) => C.UserRepository (UserApp m) where
  getUserById id = ask >>= flip UR.getUserById id
  getUserByUsername username = ask >>= flip UR.getUserByUsername username
  getAllUsers = ask >>= UR.getAllUsers
  createUser uData = ask >>= flip UR.createUser uData
  saveUser u = ask >>= flip UR.saveUser u
  deleteUser id = ask >>= flip UR.deleteUser id
