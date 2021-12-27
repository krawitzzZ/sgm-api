module Configuration
  ( mkAppEnv
  , loadEnv
  ) where

import           Configuration.Dotenv                               ( defaultConfig
                                                                    , defaultValidatorMap
                                                                    , loadSafeFile
                                                                    )
import           Control.Exception.Safe                             ( SomeException(..)
                                                                    , try
                                                                    )
import           Data.Map.Strict                                    ( empty )
import           Data.Password.Validate                             ( PasswordPolicy(..)
                                                                    , ValidPasswordPolicy
                                                                    , defaultPasswordPolicy
                                                                    , validatePasswordPolicy
                                                                    )
import           Data.String.Conversions                            ( cs )
import           Database.Beam.Postgres                             ( Connection
                                                                    , connectPostgreSQL
                                                                    )
import           Di.Core                                            ( Di )
import           Domain.App.Config                                  ( Config(..) )
import           Domain.App.Env                                     ( Env(..) )
import           Domain.Logger                                      ( LogContext
                                                                    , LogLevel(..)
                                                                    , LogMessage
                                                                    , Logger(..)
                                                                    )
import           RIO                                                ( ($)
                                                                    , (*)
                                                                    , (.)
                                                                    , (<$>)
                                                                    , (<&>)
                                                                    , (<*>)
                                                                    , (<>)
                                                                    , (>>=)
                                                                    , Either(..)
                                                                    , Maybe(..)
                                                                    , MonadIO
                                                                    , error
                                                                    , liftIO
                                                                    , return
                                                                    , show
                                                                    , void
                                                                    )
import           RIO.Time                                           ( secondsToNominalDiffTime )
import           Servant.Auth.Server                                ( JWTSettings
                                                                    , defaultJWTSettings
                                                                    , generateKey
                                                                    , readKey
                                                                    )
import           Utils                                              ( readEnvDefault
                                                                    , readEnvText
                                                                    )


loadEnv :: MonadIO m => m ()
loadEnv = void $ loadSafeFile defaultValidatorMap "env.schema.yaml" defaultConfig

mkAppEnv :: MonadIO m => Di LogLevel LogContext LogMessage -> m Env
mkAppEnv di =
  Env <$> mkAppConfig <*> mkLogger di <*> mkJwtSettings <*> mkPasswordPolicy <*> mkConnection

mkAppConfig :: MonadIO m => m Config
mkAppConfig =
  Config
    <$> readEnvDefault "PORT" 8080
    <*> readEnvText "DB_URL"
    <*> readEnvDefault "SERVER_TIMEOUT" 20
    <*> readEnvDefault "LOG_LEVEL"      Info
    <*> return (secondsToNominalDiffTime (60 * 5))

mkLogger :: MonadIO m => Di LogLevel LogContext LogMessage -> m Logger
mkLogger lDi = return Logger { lDi, lFields = empty, lError = Nothing }

mkJwtSettings :: MonadIO m => m JWTSettings
mkJwtSettings = do
  jwtKeyFilename <- cs <$> readEnvText "JWT_SECRET_FILENAME"
  eitherKey      <- liftIO $ try $ readKey jwtKeyFilename
  case eitherKey of
    Left  (SomeException _) -> liftIO generateKey <&> defaultJWTSettings
    Right key               -> return $ defaultJWTSettings key

mkConnection :: MonadIO m => m Connection
mkConnection = liftIO $ mkAppConfig >>= connectPostgreSQL . cs . cDbUrl

mkPasswordPolicy :: MonadIO m => m ValidPasswordPolicy
mkPasswordPolicy = do
  let passwordPolicy = defaultPasswordPolicy { uppercaseChars = 1 }
  case validatePasswordPolicy passwordPolicy of
    Left  reasons -> error $ "Failed to validate password policy: " <> show reasons
    Right policy  -> return policy
