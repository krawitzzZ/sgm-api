module Domain.Logger.Class
  ( MonadLogger(..)
  ) where

import           RIO                                      ( Show
                                                          , Text
                                                          )


class MonadLogger m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logWarn :: Text -> m ()
  logError :: Text -> m ()
  withContext :: Show path => path -> m a -> m a
  withError :: Show err => err -> m a -> m a
  withField :: Show s => (s, s) -> m a -> m a
  withFields :: Show s => [(s, s)] -> m a -> m a
