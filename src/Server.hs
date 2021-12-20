module Server
  ( start
  ) where

import           Api                                      ( SGMApi
                                                          , server
                                                          )
import           Data.Default                             ( def )
import           Data.Map.Strict                          ( fromList )
import           Data.String.Conversions                  ( cs )
import           Di.Core                                  ( log )
import           Domain.App                               ( runAppT )
import           Domain.Config                            ( Config(..) )
import           Domain.Env                               ( Env(..) )
import           Domain.Logger                            ( LogLevel(..)
                                                          , LogMessage(..)
                                                          , Logger(..)
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
                                                          , (=<<)
                                                          , IO
                                                          , Maybe(..)
                                                          , liftIO
                                                          , return
                                                          , show
                                                          )
import           Servant                                  ( Context(..)
                                                          , ErrorFormatters(..)
                                                          , NotFoundErrorFormatter
                                                          , Proxy(..)
                                                          , defaultErrorFormatters
                                                          , err404
                                                          , errBody
                                                          , hoistServerWithContext
                                                          , serveWithContext
                                                          )
import           Servant.Auth.Server                      ( CookieSettings
                                                          , JWT
                                                          , JWTSettings
                                                          , defaultCookieSettings
                                                          )


start :: Env -> IO ()
start env = do
  requestLoggerMw <- mkJSONRequestLoggerMiddleware
  let di       = loggerDi . envLogger $ env
  let config   = envConfig env
  let port     = configPort config
  let logLevel = configLogLevel config
  let timeout  = configNetworkTimeout config
  let fields = fromList
        [ ("logLevel", cs . show $ logLevel)
        , ("port"    , cs . show $ port)
        , ("timeout" , cs . show $ timeout)
        ]

  log di Info LogMessage { lmMessage = "Starting SGM API", lmError = Nothing, lmFields = fields }

  let settings   = setPort port . setTimeout timeout $ defaultSettings
  let middleware = requestLoggerMw . errorMwDefJson
  runSettings settings . middleware =<< mkApp env

serverApi :: Proxy (SGMApi '[JWT])
serverApi = Proxy :: Proxy (SGMApi '[JWT])

contextApi :: Proxy '[CookieSettings , JWTSettings]
contextApi = Proxy :: Proxy '[CookieSettings , JWTSettings]

mkApp :: Env -> IO Application
mkApp env = return $ serve $ hoistServer server
 where
  serve       = serveWithContext serverApi context
  hoistServer = hoistServerWithContext serverApi contextApi (liftIO . runAppT env)
  context     = customFormatters :. cookieConf :. jwtConf :. EmptyContext
  cookieConf  = defaultCookieSettings
  jwtConf     = envJwtSettings env

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req = err404 { errBody = cs $ "Not found path: " <> rawPathInfo req }

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters { notFoundErrorFormatter = notFoundFormatter }

mkJSONRequestLoggerMiddleware :: IO Middleware
mkJSONRequestLoggerMiddleware =
  mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
