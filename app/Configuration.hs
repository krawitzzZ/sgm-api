module Configuration
  ( mkAppEnv
  , loadEnv
  ) where

import           Configuration.Dotenv                     ( defaultConfig
                                                          , defaultValidatorMap
                                                          , loadSafeFile
                                                          )
import           Domain.App                               ( AppEnv(..)
                                                          , Config(..)
                                                          )
import           Domain.Logger                            ( LogLevel(..)
                                                          , Logger(..)
                                                          )
import           Domain.User                              ( UserRepository(..) )
import qualified Infra.UserRepository                    as UR
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<$>)
                                                          , (<*>)
                                                          , MonadIO
                                                          , Text
                                                          , liftIO
                                                          , return
                                                          , void
                                                          )
import           System.Log.FastLogger                    ( defaultBufSize
                                                          , newStderrLoggerSet
                                                          , pushLogStrLn
                                                          , toLogStr
                                                          )
import           Utils                                    ( readEnv )


loadEnv :: MonadIO m => m ()
loadEnv = void $ loadSafeFile defaultValidatorMap "env.schema.yaml" defaultConfig

mkAppEnv :: MonadIO m => Text -> m AppEnv
mkAppEnv version = AppEnv <$> mkAppConfig version <*> mkAppLogger <*> mkUserRepository

mkAppConfig :: MonadIO m => Text -> m Config
mkAppConfig version =
  Config
    <$> readEnv "PORT"           8080
    <*> readEnv "SERVER_TIMEOUT" 20
    <*> return version
    <*> readEnv "LOG_LEVEL" Debug

mkAppLogger :: MonadIO m => m Logger
mkAppLogger = do
  stdErrLogger <- liftIO $ newStderrLoggerSet defaultBufSize
  return Logger { logMsg = pushLogStrLn stdErrLogger . toLogStr }

mkUserRepository :: (MonadIO m) => m UserRepository
mkUserRepository = return UserRepository { findOne   = UR.findOne
                                         , get       = UR.get
                                         , saveOne   = UR.saveOne
                                         , upsertOne = UR.upsertOne
                                         , deleteOne = UR.deleteOne
                                         }
