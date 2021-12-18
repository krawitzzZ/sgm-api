module Main where

import           Configuration                            ( loadEnv
                                                          , mkAppEnv
                                                          )
import           Data.Map.Strict                          ( empty )
import           Data.String.Conversions                  ( cs )
import           Di.Core                                  ( log
                                                          , new
                                                          , push
                                                          )
import           Domain.Env                               ( Env(envDbConn) )
import           Domain.Logger                            ( LogLevel(..)
                                                          , LogMessage(..)
                                                          )
import           Infra.Beam.Schema                        ( migrateSgmDb )
import           Infra.Logger.StdErr                      ( mkDiLogFunc )
import           RIO                                      ( ($)
                                                          , (=<<)
                                                          , (>>=)
                                                          , IO
                                                          , Maybe(..)
                                                          )
import           Server                                   ( start )
import           Utils                                    ( readEnvDefault )


main :: IO ()
main = do
  loadEnv

  loggingFunction <- mkDiLogFunc =<< readEnvDefault "LOG_LEVEL" Info

  new loggingFunction $ \di' -> do
    let di = push "SGM-API" di'

    mkAppEnv di >>= \env -> do
      migrateSgmDb (envDbConn env) $ \msg -> log di Debug $ LogMessage (cs msg) empty Nothing
      start env
