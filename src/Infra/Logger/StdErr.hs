module Infra.Logger.StdErr
  ( mkDiLogFunc
  ) where

import           Data.Sequence                            ( intersperse )
import           Data.Time.Clock.System                   ( systemToUTCTime )
import           Di.Core                                  ( Log(..) )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Domain.Logger.LogMessage                 ( LogMessage(..)
                                                          , LogPath
                                                          )
import           Infra.Logger.LogLine                     ( LogLine(..) )
import           RIO                                      ( ($)
                                                          , (<&>)
                                                          , (>=)
                                                          , IO
                                                          , MonadIO
                                                          , liftIO
                                                          , mconcat
                                                          , toList
                                                          , when
                                                          )
import           System.Log.FastLogger                    ( LoggerSet
                                                          , ToLogStr(..)
                                                          , defaultBufSize
                                                          , newStderrLoggerSet
                                                          , pushLogStrLn
                                                          )


mkDiLogFunc :: MonadIO m => LogLevel -> m (Log LogLevel LogPath LogMessage -> IO ())
mkDiLogFunc appLogLevel = liftIO $ newStderrLoggerSet defaultBufSize <&> printToStdErr
 where
  printToStdErr :: LoggerSet -> Log LogLevel LogPath LogMessage -> IO ()
  printToStdErr logger Log { log_time, log_level, log_path, log_message } = do
    when (log_level >= appLogLevel) $ do
      let logLine = LogLine { time    = systemToUTCTime log_time
                            , level   = log_level
                            , message = lmMessage log_message
                            , fields  = lmFields log_message
                            , error   = lmError log_message
                            , context = mconcat $ toList (intersperse " => " log_path)
                            }
      liftIO $ pushLogStrLn logger $ toLogStr logLine
