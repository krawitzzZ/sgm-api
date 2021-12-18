module Infra.Logger.StdErr
  ( mkDiLogFunc
  ) where

import           Data.Map.Strict                          ( size )
import           Data.Sequence                            ( intersperse )
import           Data.Time.Clock.System                   ( systemToUTCTime )
import           Di.Core                                  ( Log(..) )
import qualified Domain.Logger                           as DL
import           Infra.Logger.LogLine                     ( LogLine(..)
                                                          , fromDomain
                                                          )
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


mkDiLogFunc :: MonadIO m => DL.LogLevel -> m (Log DL.LogLevel DL.LogContext DL.LogMessage -> IO ())
mkDiLogFunc appLogLevel = liftIO $ newStderrLoggerSet defaultBufSize <&> printToStdErr
 where
  printToStdErr :: LoggerSet -> Log DL.LogLevel DL.LogContext DL.LogMessage -> IO ()
  printToStdErr logger Log { log_time, log_level, log_path, log_message } = do
    when (log_level >= appLogLevel) $ do
      let time     = systemToUTCTime log_time
      let severity = fromDomain log_level
      let message  = DL.lmMessage log_message
      let error    = DL.lmError log_message
      let fields   = getFields $ DL.lmFields log_message
      let context  = mconcat $ toList (intersperse " => " log_path)
      let logLine  = LogLine { time, severity, message, fields, error, context }
      liftIO $ pushLogStrLn logger $ toLogStr logLine

  getFields fs | size fs == 0 = Nothing
               | otherwise    = Just fs
