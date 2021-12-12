module Server
  ( start
  ) where

import           Api                                      ( SGMApi
                                                          , server
                                                          )
import           Data.Default                             ( def )
import           Data.Map.Strict                          ( empty )
import           Data.String.Conversions                  ( cs )
import           Di.Core                                  ( log )
import           Domain.App                               ( runAppT )
import           Domain.Config                            ( Config(..) )
import           Domain.Env                               ( Env(..) )
import           Domain.Logger                            ( Logger(..) )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Domain.Logger.LogMessage                 ( LogMessage(..) )
import           Network.Wai                              ( Application
                                                          , Middleware
                                                          , rawPathInfo
                                                          )
import           Network.Wai.Handler.Warp                 ( defaultSettings
                                                          , runSettings
                                                          , setPort
                                                          , setTimeout
                                                          )
import           Network.Wai.Middleware.RequestLogger     ( OutputFormat(..)
                                                          , RequestLoggerSettings(..)
                                                          , mkRequestLogger
                                                          )
import           Network.Wai.Middleware.RequestLogger.JSON
                                                          ( formatAsJSON )
import           Network.Wai.Middleware.Servant.Errors    ( errorMwDefJson )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<>)
                                                          , IO
                                                          , Maybe(..)
                                                          , liftIO
                                                          , show
                                                          )
import           Servant                                  ( Context(..)
                                                          , ErrorFormatters(..)
                                                          , NotFoundErrorFormatter
                                                          , Proxy(..)
                                                          , defaultErrorFormatters
                                                          , err404
                                                          , errBody
                                                          , hoistServer
                                                          , serveWithContext
                                                          )


start :: Env -> IO ()
start env = do
  requestLoggerMw <- mkJSONRequestLoggerMiddleware
  let config  = envConfig env
  let port    = configPort config
  let timeout = configNetworkTimeout config
  let di      = loggerDi . envLogger $ env
  let initMessage = LogMessage { lmMessage = cs $ "Starting SGM API on port " <> show port
                               , lmError   = Nothing
                               , lmFields  = empty
                               }

  log di Info initMessage

  let settings = setPort port . setTimeout timeout $ defaultSettings
  runSettings settings . requestLoggerMw . errorMwDefJson . mkApp $ env

api :: Proxy SGMApi
api = Proxy :: Proxy SGMApi

mkApp :: Env -> Application
mkApp env = serveWithContext api (customFormatters :. EmptyContext)
  $ hoistServer api (liftIO . runAppT env) server

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req = err404 { errBody = cs $ "Not found path: " <> rawPathInfo req }

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters { notFoundErrorFormatter = notFoundFormatter }

mkJSONRequestLoggerMiddleware :: IO Middleware
mkJSONRequestLoggerMiddleware =
  mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
