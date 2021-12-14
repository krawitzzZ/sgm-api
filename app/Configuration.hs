module Configuration
  ( mkAppEnv
  , loadEnv
  ) where

import           Configuration.Dotenv                     ( defaultConfig
                                                          , defaultValidatorMap
                                                          , loadSafeFile
                                                          )
import           Data.Map.Strict                          ( empty )
import           Database.Beam                            ( DatabaseSettings )
import           Database.Beam.Postgres                   ( Postgres )
import           Di.Core                                  ( Di )
import           Domain.Config                            ( Config(..) )
import           Domain.Env                               ( Env(..) )
import           Domain.Logger                            ( Logger(..) )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Domain.Logger.LogMessage                 ( LogMessage
                                                          , LogPath
                                                          )
import           Infra.Db.Schema                          ( SgmDatabase )
import           RIO                                      ( ($)
                                                          , (<$>)
                                                          , (<*>)
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

mkAppEnv
  :: MonadIO m => Di LogLevel LogPath LogMessage -> DatabaseSettings Postgres SgmDatabase -> m Env
mkAppEnv di db = Env <$> mkAppConfig <*> mkLogger di <*> return db

mkAppConfig :: MonadIO m => m Config
mkAppConfig =
  Config
    <$> readEnvDefault "PORT" 8080
    <*> readEnvText "DB_URL"
    <*> readEnvDefault "SERVER_TIMEOUT" 20
    <*> readEnvDefault "LOG_LEVEL"      Info

mkLogger :: MonadIO m => Di LogLevel LogPath LogMessage -> m Logger
mkLogger loggerDi = return Logger { loggerDi, loggerFields = empty, loggerError = Nothing }
