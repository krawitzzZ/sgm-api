module Infra.Logger
  ( logDebug
  , logInfo
  , logWarn
  , logError
  , withContext
  , withError
  , withField
  , withFields
  ) where

import           Control.Monad.Reader.Has                           ( Has
                                                                    , extract
                                                                    , update
                                                                    )
import           Data.Map.Strict                                    ( fromList
                                                                    , singleton
                                                                    )
import           Data.String.Conversions                            ( cs )
import           Di.Monad                                           ( MonadDi
                                                                    , log
                                                                    , push
                                                                    )
import           Domain.Logger                                      ( LogContext
                                                                    , LogLevel(..)
                                                                    , LogMessage(..)
                                                                    , Logger(..)
                                                                    )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<>)
                                                                    , (>>=)
                                                                    , Maybe(..)
                                                                    , MonadReader
                                                                    , Show
                                                                    , Text
                                                                    , asks
                                                                    , const
                                                                    , local
                                                                    , show
                                                                    )


logDebug
  :: (Has Logger e, MonadReader e m, MonadDi LogLevel LogContext LogMessage m) => Text -> m ()
logDebug msg = asks extract >>= logMessage Debug msg

logInfo
  :: (Has Logger e, MonadReader e m, MonadDi LogLevel LogContext LogMessage m) => Text -> m ()
logInfo msg = asks extract >>= logMessage Info msg

logWarn
  :: (Has Logger e, MonadReader e m, MonadDi LogLevel LogContext LogMessage m) => Text -> m ()
logWarn msg = asks extract >>= logMessage Warn msg

logError
  :: (Has Logger e, MonadReader e m, MonadDi LogLevel LogContext LogMessage m) => Text -> m ()
logError msg = asks extract >>= logMessage Error msg

withContext :: (MonadDi LogLevel LogContext LogMessage m) => LogContext -> m a -> m a
withContext = push

withError :: (Has Logger e, MonadReader e m, Show err) => err -> m a -> m a
withError err action = do
  logger <- asks extract
  local (update . const $ logger { lError = Just . cs . show $ err }) action

withField :: (Has Logger e, MonadReader e m) => (Text, Text) -> m a -> m a
withField (field, value) action = do
  logger <- asks extract
  let fields = lFields logger <> singleton field value
  local (update . const $ logger { lFields = fields }) action

withFields :: (Has Logger e, MonadReader e m) => [(Text, Text)] -> m a -> m a
withFields fields action = do
  logger <- asks extract
  let fields' = lFields logger <> fromList fields
  local (update . const $ logger { lFields = fields' }) action

logMessage :: (MonadDi LogLevel LogContext LogMessage m) => LogLevel -> Text -> Logger -> m ()
logMessage level lmMessage Logger {..} =
  log level LogMessage { lmMessage, lmFields = lFields, lmError = lError }
