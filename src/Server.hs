module Server
  ( start
  ) where

import           Api                                      ( SGMApi
                                                          , server
                                                          )
import           Data.Default                             ( def )
import           Data.String.Conversions                  ( cs )
import           Domain.App                               ( AppEnv(..)
                                                          , Config(..)
                                                          )
import           Domain.Logger                            ( HasLogger(..)
                                                          , LogLevel(..)
                                                          , LogMessage(..)
                                                          )
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
                                                          , runReaderT
                                                          , show
                                                          )
import           RIO.Text                                 ( pack )
import           RIO.Time                                 ( getCurrentTime )
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


start :: AppEnv -> IO ()
start env = do
  time            <- getCurrentTime
  requestLoggerMw <- mkJSONRequestLoggerMiddleware
  let config         = appConfig env
  let port           = configPort config
  let timeout        = configNetworkTimeout config
  let logInitMessage = getLogFunc env
  let initMsg = LogMessage { time
                           , level   = Info
                           , message = pack $ "Starting SGM API on port " <> show port
                           , context = "Server->start"
                           , version = configVersion config
                           }

  logInitMessage initMsg

  let settings = setPort port . setTimeout timeout $ defaultSettings
  runSettings settings . requestLoggerMw . errorMwDefJson . mkApp $ env

api :: Proxy SGMApi
api = Proxy :: Proxy SGMApi

mkApp :: AppEnv -> Application
mkApp env = serveWithContext api (customFormatters :. EmptyContext)
  $ hoistServer api (`runReaderT` env) server

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req = err404 { errBody = cs $ "Not found path: " <> rawPathInfo req }

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters { notFoundErrorFormatter = notFoundFormatter }

mkJSONRequestLoggerMiddleware :: IO Middleware
mkJSONRequestLoggerMiddleware =
  mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
