module Domain.Logger.Class
  ( IsLogger(..)
  ) where

import           Domain.Logger.LogMessage                 ( LogMessage(..) )
import           RIO                                      ( IO
                                                          , Show
                                                          , Text
                                                          )


class IsLogger logger where
  log :: logger -> LogMessage -> IO ()
  withError :: (Show e) => e -> logger -> logger
  withContext :: Text -> logger -> logger
  withErrorAndContext :: (Show e) => e -> Text -> logger -> logger
