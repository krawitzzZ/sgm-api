module Domain.Logger
  ( Logger(..)
  , HasLogger(..)
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import           Data.String.Conversions                  ( cs )
import           Domain.Config                            ( Config(..)
                                                          , HasConfig(..)
                                                          )
import           Domain.Logger.Class                      ( IsLogger(..) )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Domain.Logger.LogMessage                 ( LogMessage(..) )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<$>)
                                                          , (>=)
                                                          , IO
                                                          , Maybe(..)
                                                          , MonadIO
                                                          , MonadReader
                                                          , Show
                                                          , Text
                                                          , asks
                                                          , id
                                                          , liftIO
                                                          , show
                                                          , when
                                                          )
import           RIO.Time                                 ( getCurrentTime )


data Logger = Logger
  { lContext :: !Text
  , lError   :: !(Maybe Text)
  , logMsg   :: !(LogMessage -> IO ())
  }
class HasLogger env where
  {-# MINIMAL getLogger #-}
  getLogger :: env -> Logger
  getLogFunc :: env -> LogMessage -> IO ()
  getLogFunc  = logMsg . getLogger
instance HasLogger Logger where
  getLogger = id
instance IsLogger Logger where
  log logger = logMsg logger
  withError err logger = logger { lError = justError err }
  withContext lContext logger = logger { lContext }
  withErrorAndContext err lContext logger = logger { lContext, lError = justError err }

logDebug :: (MonadIO m, MonadReader env m, HasConfig env) => Logger -> Text -> m ()
logDebug = logGeneral Debug

logInfo :: (MonadIO m, MonadReader env m, HasConfig env) => Logger -> Text -> m ()
logInfo = logGeneral Info

logWarn :: (MonadIO m, MonadReader env m, HasConfig env) => Logger -> Text -> m ()
logWarn = logGeneral Warn

logError :: (MonadIO m, MonadReader env m, HasConfig env) => Logger -> Text -> m ()
logError = logGeneral Error

logGeneral :: (MonadIO m, MonadReader env m, HasConfig env) => LogLevel -> Logger -> Text -> m ()
logGeneral level logger message = do
  appLogLevel <- asks $ configLogLevel . getConfig
  when (level >= appLogLevel) $ do
    time <- getCurrentTime
    let error      = cs . show <$> lError logger
    let context    = lContext logger
    let logMessage = logMsg logger
    let msg        = LogMessage { time, level, message, error, context }
    liftIO $ logMessage msg

justError :: Show e => e -> Maybe Text
justError = Just . cs . show
