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
import qualified Domain.Auth.Password                    as Password
import           Domain.Class                             ( Authentication(..)
                                                          , EventRepository(..)
                                                          , MonadLogger(..)
                                                          , UserRepository(..)
                                                          )
import           Domain.Config                            ( Config(..) )
import           Domain.Env                               ( Env(..) )
import           Domain.Logger                            ( LogContext
                                                          , LogLevel
                                                          , LogMessage
                                                          , Logger(..)
                                                          )
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
  getUserById id = asks envDbConn >>= flip UserRepo.findOneById id
  getUserByUsername username = asks envDbConn >>= flip UserRepo.findOneByUsername username
  getAllUsers = asks envDbConn >>= UserRepo.getAll
  createUser uData = asks envDbConn >>= flip UserRepo.createOne uData
  saveUser u = asks envDbConn >>= flip UserRepo.saveOne u
  deleteUser id = asks envDbConn >>= flip UserRepo.deleteOne id

instance (MonadIO m, MonadCatch m) => EventRepository (AppT m) where
  getEventById id = asks envDbConn >>= flip EventRepo.findOneById id
  getAllEvents = asks envDbConn >>= EventRepo.getAll
  createEvent eData = asks envDbConn >>= flip EventRepo.createOne eData
  saveEvent e = asks envDbConn >>= flip EventRepo.saveOne e
  deleteEvent id = asks envDbConn >>= flip EventRepo.deleteOne id

instance (MonadIO m, MonadTime m) => Authentication (AppT m) where
  validatePassword pass = asks extract >>= Password.validatePassword pass
  checkPassword = Password.checkPassword
  createJwt user = do
    tokenDuration <- cJwtDuration <$> asks extract
    jwtSettings   <- asks extract
    Auth.mkJwt tokenDuration jwtSettings user
