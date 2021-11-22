module Configuration
  ( mkAppEnv
  , loadEnv
  ) where

import           Configuration.Dotenv                     ( defaultConfig
                                                          , defaultValidatorMap
                                                          , loadSafeFile
                                                          )
import           Domain.App                               ( AppEnv(..) )
import           Domain.Config                            ( Config(..) )
import           Domain.Logger                            ( Logger(..) )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Domain.User                              ( UserRepository(..) )
import           Infra.Logger.StdErr                      ( mkLogFunc )
import qualified Infra.UserRepository                    as UR
import           RIO                                      ( ($)
                                                          , (<$>)
                                                          , (<*>)
                                                          , (>>=)
                                                          , Maybe(..)
                                                          , MonadIO
                                                          , return
                                                          , void
                                                          )
import           Utils                                    ( readEnvDefault
                                                          , readEnvText
                                                          )


loadEnv :: MonadIO m => m ()
loadEnv = void $ loadSafeFile defaultValidatorMap "env.schema.yaml" defaultConfig

mkAppEnv :: MonadIO m => m AppEnv
mkAppEnv = AppEnv <$> mkAppConfig <*> mkAppLogger <*> mkUserRepository

mkAppConfig :: MonadIO m => m Config
mkAppConfig =
  Config
    <$> readEnvDefault "PORT" 8080
    <*> readEnvText "DB_URL"
    <*> readEnvDefault "SERVER_TIMEOUT" 20
    <*> readEnvDefault "LOG_LEVEL"      Info

mkAppLogger :: MonadIO m => m Logger
mkAppLogger = mkLogFunc >>= \logToStdErr ->
  return Logger { logMsg = logToStdErr, lContext = "Default", lError = Nothing }

mkUserRepository :: (MonadIO m) => m UserRepository
mkUserRepository = return UserRepository { findOne   = UR.findOne
                                         , get       = UR.get
                                         , saveOne   = UR.saveOne
                                         , upsertOne = UR.upsertOne
                                         , deleteOne = UR.deleteOne
                                         }
