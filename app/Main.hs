module Main where

import           Configuration                                      ( loadEnv
                                                                    , mkAppEnv
                                                                    )
import           Di.Core                                            ( new
                                                                    , push
                                                                    )
import           Domain.App.Env                                     ( Env(..) )
import           Domain.Logger                                      ( LogLevel(..) )
import           Infra.Beam.Schema                                  ( migrateSgmDb )
import           Infra.Logger.StdErr                                ( mkDiLogFunc )
import           Prelude                                            ( putStrLn )
import           RIO                                                ( ($)
                                                                    , (=<<)
                                                                    , (>>)
                                                                    , (>>=)
                                                                    , IO
                                                                    )
import           Server                                             ( start )
import           Utils                                              ( readEnvDefault )


main :: IO ()
main = do
  loadEnv

  loggingFunction <- mkDiLogFunc =<< readEnvDefault "LOG_LEVEL" Info

  new loggingFunction $ \di' -> do
    let di = push "SGM-API" di'
    mkAppEnv di >>= \env -> migrateSgmDb (envDbConn env) putStrLn >> start env
