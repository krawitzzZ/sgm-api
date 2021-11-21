module Main where

import           Configuration                            ( loadEnv
                                                          , mkAppEnv
                                                          )
import           RIO                                      ( (>>)
                                                          , (>>=)
                                                          , IO
                                                          )
import           Server                                   ( start )


main :: IO ()
main = loadEnv >> mkAppEnv "0.1.0.0" >>= start
