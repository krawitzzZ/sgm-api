module Infra.Logger
  ( logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import           Domain.App                               ( Config(..)
                                                          , HasConfig(..)
                                                          )
import           Domain.Logger                            ( HasLogger(..)
                                                          , LogLevel(..)
                                                          , LogMessage(..)
                                                          )
import           RIO                                      ( ($)
                                                          , (>=)
                                                          , MonadIO(..)
                                                          , MonadReader
                                                          , Text
                                                          , asks
                                                          , when
                                                          )
import           RIO.Time                                 ( getCurrentTime )


logDebug :: (MonadIO m, MonadReader env m, HasLogger env, HasConfig env) => Text -> Text -> m ()
logDebug = logGeneral Debug

logInfo :: (MonadIO m, MonadReader env m, HasLogger env, HasConfig env) => Text -> Text -> m ()
logInfo = logGeneral Info

logWarn :: (MonadIO m, MonadReader env m, HasLogger env, HasConfig env) => Text -> Text -> m ()
logWarn = logGeneral Warn

logError :: (MonadIO m, MonadReader env m, HasLogger env, HasConfig env) => Text -> Text -> m ()
logError = logGeneral Error

logGeneral
  :: (MonadIO m, MonadReader env m, HasLogger env, HasConfig env)
  => LogLevel
  -> Text
  -> Text
  -> m ()
logGeneral level ctx msg = do
  appConfig <- asks getConfig
  let appLogLevel = configLogLevel appConfig
  when (level >= appLogLevel) $ do
    time   <- getCurrentTime
    logMsg <- asks getLogFunc
    let logMessage = LogMessage { time
                                , level
                                , message = msg
                                , context = ctx
                                , version = configVersion appConfig
                                }
    liftIO $ logMsg logMessage
