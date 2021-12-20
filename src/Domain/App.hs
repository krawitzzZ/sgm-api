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
import           Domain.Class                             ( MonadLogger(..)
                                                          , UserRepository(..)
                                                          )
import           Domain.Env                               ( Env )
import           Domain.Logger                            ( LogContext
                                                          , LogLevel
                                                          , LogMessage
                                                          , Logger(..)
                                                          )
import qualified Infra.Logger                            as L
import qualified Infra.UserRepository                    as UR
import           RIO                                      ( (.)
                                                          , Applicative
                                                          , Functor
                                                          , Generic
                                                          , Monad
                                                          , MonadIO
                                                          , MonadReader
                                                          , MonadTrans(..)
                                                          , ReaderT(..)
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
  logDebug    = L.logDebug
  logInfo     = L.logInfo
  logWarn     = L.logWarn
  logError    = L.logError
  withContext = L.withContext
  withError   = L.withError
  withField   = L.withField
  withFields  = L.withFields

instance (MonadIO m, MonadCatch m) => UserRepository (AppT m) where
  getUserById       = UR.findOneById
  getUserByUsername = UR.findOneByUsername
  getAllUsers       = UR.getAll
  createUser        = UR.createOne
  saveUser          = UR.saveOne
  deleteUser        = UR.deleteOne
