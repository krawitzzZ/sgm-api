module Main where

import           Configuration                            ( loadEnv
                                                          , mkAppEnv
                                                          )
import           Data.Map.Strict                          ( empty )
import           Data.String.Conversions                  ( cs )
import           Database.Beam.Postgres                   ( connectPostgreSQL
                                                          , runBeamPostgres
                                                          , runBeamPostgresDebug
                                                          )
import           Di.Core                                  ( log
                                                          , new
                                                          , push
                                                          )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Domain.Logger.LogMessage                 ( LogMessage(..) )
import           Infra.Db.Schema                          ( getSgmDatabase )
import           Infra.Logger.StdErr                      ( mkDiLogFunc )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<&>)
                                                          , (>>=)
                                                          , IO
                                                          , Maybe(..)
                                                          , liftIO
                                                          )
import           Server                                   ( start )
import           Utils                                    ( readEnvDefault
                                                          , readEnvText
                                                          )


main :: IO ()
main = do
  loadEnv

  logLevel        <- readEnvDefault "LOG_LEVEL" Info
  dbConnStr       <- readEnvText "DB_URL" <&> cs
  loggingFunction <- mkDiLogFunc logLevel

  new loggingFunction $ \di' -> do
    let di = push "SGM-API.Main->main" di'
    let dbLog msg = log di Info $ LogMessage (cs msg) empty Nothing

    connectPostgreSQL dbConnStr >>= runWithDb logLevel dbLog (startApp di)
 where
  startApp di db = mkAppEnv di db >>= liftIO . start
  runWithDb logLevel logFunc action conn =
    runBeam logLevel conn $ getSgmDatabase logFunc >>= action
   where
    runBeam Debug = runBeamPostgresDebug logFunc
    runBeam _     = runBeamPostgres
