module Infra.Logger.StdErr
  ( mkDiLogFunc
  ) where

import           Data.Map.Strict                          ( size )
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
                                                          , (==)
                                                          , (>=)
                                                          , IO
                                                          , Maybe(..)
                                                          , MonadIO
                                                          , liftIO
                                                          , mconcat
                                                          , otherwise
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
      let time    = systemToUTCTime log_time
      let message = lmMessage log_message
      let error   = lmError log_message
      let fields  = getFields $ lmFields log_message
      let context = mconcat $ toList (intersperse " => " log_path)
      let logLine = LogLine { time, level = log_level, message, fields, error, context }
      liftIO $ pushLogStrLn logger $ toLogStr logLine

  getFields fs | size fs == 0 = Nothing
               | otherwise    = Just fs
