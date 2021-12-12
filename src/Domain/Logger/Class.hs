module Domain.Logger.Class
  ( MonadLogger(..)
  ) where

import           Domain.Logger.LogMessage                 ( LogPath )
import           RIO                                      ( Show
                                                          , Text
                                                          )


class MonadLogger m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logWarn :: Text -> m ()
  logError :: Text -> m ()
  withContext :: LogPath -> m a -> m a
  withError :: Show err => err -> m a -> m a
  withField :: Show s => (s, s) -> m a -> m a
  withFields :: Show s => [(s, s)] -> m a -> m a
