module Main where

import           Configuration                            ( loadEnv
                                                          , mkAppEnv
                                                          )
import           Di.Core                                  ( new
                                                          , push
                                                          )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Infra.Logger.StdErr                      ( mkDiLogFunc )
import           RIO                                      ( ($)
                                                          , (>>=)
                                                          , IO
                                                          )
import           Server                                   ( start )
import           Utils                                    ( readEnvDefault )


main :: IO ()
main = do
  loadEnv
  logToStdErr <- readEnvDefault "LOG_LEVEL" Info >>= mkDiLogFunc
  new logToStdErr $ \di -> mkAppEnv (push "SGM-API.Main->main" di) >>= start
