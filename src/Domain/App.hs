module Domain.App
  ( AppT(..)
  , runAppT
  ) where

import           Control.Exception.Safe                   ( MonadCatch
                                                          , MonadThrow
                                                          )
import           Control.Monad.Reader.Has                 ( Has
                                                          , extract
                                                          )
import           Control.Monad.Time                       ( MonadTime )
import           Di.Monad                                 ( DiT
                                                          , MonadDi
                                                          , runDiT
                                                          )
import qualified Domain.Auth                             as Auth
import           Domain.Class                             ( Authentication(..)
                                                          , EventRepository(..)
                                                          , MonadLogger(..)
                                                          , UserRepository(..)
                                                          )
import           Domain.Config                            ( Config(..) )
import           Domain.Env                               ( Env )
import           Domain.Logger                            ( LogContext
                                                          , LogLevel
                                                          , LogMessage
                                                          , Logger(..)
                                                          )
import qualified Domain.Password                         as Password
import qualified Infra.EventRepository                   as EventRepo
import qualified Infra.Logger                            as Logger
import qualified Infra.UserRepository                    as UserRepo
import           RIO                                      ( (.)
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
                                                          , asks
                                                          , flip
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

instance (MonadIO m, MonadCatch m) => UserRepository (AppT m) where
  getUserById       = UserRepo.findOneById
  getUserByUsername = UserRepo.findOneByUsername
  getAllUsers       = UserRepo.getAll
  createUser        = UserRepo.createOne
  saveUser          = UserRepo.saveOne
  deleteUser        = UserRepo.deleteOne

instance (MonadIO m, MonadCatch m) => EventRepository (AppT m) where
  getEventById = EventRepo.findOneById
  getAllEvents = EventRepo.getAll
  createEvent  = EventRepo.createOne
  saveEvent    = EventRepo.saveOne
  deleteEvent  = EventRepo.deleteOne

instance (MonadIO m, MonadTime m) => Authentication (AppT m) where
  validatePassword pass = asks extract >>= Password.validatePassword pass
  checkPassword = Password.checkPassword
  createJwt user = do
    tokenDuration <- cJwtDuration <$> asks extract
    jwtSettings   <- asks extract
    Auth.mkJwt tokenDuration jwtSettings user
