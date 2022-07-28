module Domain.App
  ( AppT(..)
  , runAppT
  ) where

import           Control.Exception.Safe                             ( MonadCatch
                                                                    , MonadThrow
                                                                    , throwM
                                                                    )
import           Control.Monad.Reader.Has                           ( Has
                                                                    , extract
                                                                    )
import           Control.Monad.Time                                 ( MonadTime )
import           Di.Monad                                           ( DiT
                                                                    , MonadDi
                                                                    , runDiT
                                                                    )
import           Domain.App.Class                                   ( AccessPolicyGuard(..)
                                                                    , Authentication(..)
                                                                    , EventRepository(..)
                                                                    , MonadLogger(..)
                                                                    , UserRepository(..)
                                                                    )
import           Domain.App.Config                                  ( Config(..) )
import           Domain.App.Env                                     ( Env(..) )
import qualified Domain.Auth                                       as Auth
import qualified Domain.Auth.Password                              as Password
import           Domain.Auth.Permission                             ( isPermitted )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.Logger                                      ( LogContext
                                                                    , LogLevel
                                                                    , LogMessage
                                                                    , Logger(..)
                                                                    )
import           Domain.Policy                                      ( HasActionPolicy(..) )
import qualified Infra.EventRepository                             as EventRepo
import qualified Infra.Logger                                      as Logger
import qualified Infra.UserRepository                              as UserRepo
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<$>)
                                                                    , (>>=)
                                                                    , Applicative
                                                                    , Functor
                                                                    , Generic
                                                                    , Monad
                                                                    , MonadIO
                                                                    , MonadReader
                                                                    , MonadTrans(..)
                                                                    , ReaderT(..)
                                                                    , ask
                                                                    , asks
                                                                    , flip
                                                                    , unless
                                                                    )


newtype AppT m a = AppT { unAppT :: ReaderT Env (DiT LogLevel LogContext LogMessage m) a }
  deriving (Functor, Generic)
  deriving newtype ( Applicative
                   , Monad
                   , MonadIO
                   , MonadTime
                   , MonadThrow
                   , MonadCatch
                   , MonadReader Env
                   , MonadDi LogLevel LogContext LogMessage
                   )

runAppT :: (Has Env e, MonadIO m) => e -> AppT m a -> m a
runAppT env =
  let appEnv         = extract env
      Logger { lDi } = extract appEnv
  in  runDiT lDi . flip runReaderT appEnv . unAppT

instance MonadTrans AppT where
  lift = AppT . lift . lift

instance Monad m => MonadLogger (AppT m) where
  logDebug    = Logger.logDebug
  logInfo     = Logger.logInfo
  logWarn     = Logger.logWarn
  logError    = Logger.logError
  withContext = Logger.withContext
  withError   = Logger.withError
  withField   = Logger.withField
  withFields  = Logger.withFields

instance (MonadIO m, MonadTime m, MonadThrow m) => Authentication (AppT m) where
  validatePassword pass = asks extract >>= Password.validatePassword pass
  checkPassword = Password.checkPassword
  refreshJwt authUser = do
    tokenDuration <- cJwtDuration <$> asks extract
    jwtSettings   <- asks extract
    Auth.refreshJwt tokenDuration jwtSettings authUser
  createJwt user = do
    tokenDuration <- cJwtDuration <$> asks extract
    jwtSettings   <- asks extract
    Auth.createJwt tokenDuration jwtSettings user

instance (MonadThrow m) => AccessPolicyGuard (AppT m) where
  checkPolicy uc a = unless (isPermitted $ actionPermission uc a) $ throwM AccessPolicyViolation

instance (MonadIO m, MonadCatch m) => UserRepository (AppT m) where
  getUserById id = ask >>= flip UserRepo.findOneById id
  getUserByUsername username = ask >>= flip UserRepo.findOneByUsername username
  getAllUsers = ask >>= UserRepo.getAll
  createUser uData = ask >>= flip UserRepo.createOne uData
  saveUser u = ask >>= flip UserRepo.saveOne u
  deleteUser id = ask >>= flip UserRepo.deleteOne id

instance (MonadIO m, MonadCatch m) => EventRepository (AppT m) where
  getEventById id = ask >>= flip EventRepo.findOneById id
  getAllEvents = ask >>= EventRepo.getAll
  createEvent eData = ask >>= flip EventRepo.createOne eData
  saveEvent e = ask >>= flip EventRepo.saveOne e
  deleteEvent id = ask >>= flip EventRepo.deleteOne id
  attendEvent e userId = ask >>= \c -> EventRepo.attendOne c e userId
  unattendEvent e userId = ask >>= \c -> EventRepo.unattendOne c e userId
