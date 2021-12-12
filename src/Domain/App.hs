module Domain.App
  ( AppT(..)
  , runAppT
  ) where

import           Control.Exception.Safe                   ( MonadCatch
                                                          , MonadThrow
                                                          )
import           Control.Monad.Time                       ( MonadTime(..) )
import           Data.Map.Strict                          ( fromList
                                                          , singleton
                                                          )
import qualified Di.Monad                                as Di
import           Domain.Env                               ( Env(..)
                                                          , HasEnv(..)
                                                          , setEnvLogger
                                                          )
import           Domain.Logger                            ( Logger(..)
                                                          , logMessage
                                                          )
import           Domain.Logger.Class                      ( MonadLogger(..) )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Domain.Logger.LogMessage                 ( LogMessage
                                                          , LogPath
                                                          )
import           Domain.User                              ( UserRepository(..) )
import qualified Infra.UserRepository                    as UR
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<&>)
                                                          , (<>)
                                                          , Applicative(..)
                                                          , Functor(..)
                                                          , Generic
                                                          , Maybe(..)
                                                          , Monad(..)
                                                          , MonadIO(..)
                                                          , MonadReader
                                                          , MonadTrans(..)
                                                          , ReaderT(..)
                                                          , ask
                                                          , asks
                                                          , bimap
                                                          , flip
                                                          , local
                                                          , map
                                                          )
import           Utils                                    ( toText )


newtype AppT m a = AppT { unAppT :: ReaderT Env (Di.DiT LogLevel LogPath LogMessage m) a }
  deriving (Functor, Generic)
  deriving newtype ( Applicative
                   , Monad
                   , MonadIO
                   , MonadReader Env
                   , Di.MonadDi LogLevel LogPath LogMessage
                   , MonadTime
                   , MonadThrow
                   , MonadCatch
                   )

runAppT :: (HasEnv e, MonadIO m) => e -> AppT m a -> m a
runAppT env =
  let appEnv              = getEnv env
      Logger { loggerDi } = envLogger appEnv
  in  Di.runDiT loggerDi . flip runReaderT appEnv . unAppT

instance MonadTrans AppT where
  lift = AppT . lift . lift

instance Monad m => MonadLogger (AppT m) where
  logDebug msg = asks envLogger >>= logMessage Debug msg
  logInfo msg = asks envLogger >>= logMessage Info msg
  logWarn msg = asks envLogger >>= logMessage Warn msg
  logError msg = asks envLogger >>= logMessage Error msg
  withContext = Di.push . toText
  withError err action = do
    logger <- ask <&> envLogger
    local (setEnvLogger logger { loggerError = Just . toText $ err }) action
  withField (field, value) action = do
    logger <- ask <&> envLogger
    let fields = loggerFields logger <> singleton (toText field) (toText value)
    local (setEnvLogger logger { loggerFields = fields }) action
  withFields fields action = do
    logger <- ask <&> envLogger
    let fields' = loggerFields logger <> fromList (map (bimap toText toText) fields)
    local (setEnvLogger logger { loggerFields = fields' }) action

instance MonadThrow m => UserRepository (AppT m) where
  getUserById = UR.findOne
  getAllUsers = UR.get
  createUser  = UR.createUser
  updateUser  = UR.upsertOne
  deleteUser  = UR.deleteOne
