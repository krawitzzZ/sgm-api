module Domain.Logger
  ( Logger(..)
  , logMessage
  ) where

import           Data.Map.Strict                          ( Map )
import           Di.Core                                  ( Di )
import           Di.Monad                                 ( MonadDi
                                                          , log
                                                          )
import           Domain.Logger.LogLevel                   ( LogLevel )
import           Domain.Logger.LogMessage                 ( LogMessage(..)
                                                          , LogPath
                                                          )
import           RIO                                      ( Maybe(..)
                                                          , Text
                                                          )


data Logger = Logger
  { loggerDi     :: !(Di LogLevel LogPath LogMessage)
  , loggerFields :: !(Map Text Text)
  , loggerError  :: !(Maybe Text)
  }

logMessage :: (MonadDi LogLevel LogPath LogMessage m) => LogLevel -> Text -> Logger -> m ()
logMessage level lmMessage Logger {..} =
  log level LogMessage { lmMessage, lmFields = loggerFields, lmError = loggerError }
