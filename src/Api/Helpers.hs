module Api.Helpers
  ( tryCatch
  , tryCatches
  , tryCatchAny
  ) where

import           Control.Exception.Safe                   ( Exception
                                                          , Handler(..)
                                                          , SomeException(..)
                                                          , catches
                                                          )
import           Domain.App                               ( AppM )
import           Domain.Logger                            ( HasLogger(..)
                                                          , logWarn
                                                          )
import           Domain.Logger.Class                      ( IsLogger(..) )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , (>>>)
                                                          , Text
                                                          , asks
                                                          )
import           Servant                                  ( err500
                                                          , errBody
                                                          , throwError
                                                          )


tryCatch :: (Exception e) => Text -> AppM a -> (e -> AppM a) -> AppM a
tryCatch ctx action handler = catches action [Handler handler, catchInternalError ctx]

tryCatchAny :: Text -> AppM a -> AppM a
tryCatchAny ctx action = catches action [catchInternalError ctx]

tryCatches :: Text -> AppM a -> [Handler AppM a] -> AppM a
tryCatches ctx action handlers = catches action (handlers <> [catchInternalError ctx])

catchInternalError :: Text -> Handler AppM a
catchInternalError ctx = Handler $ \(SomeException e) -> do
  logger <- asks $ getLogger >>> withErrorAndContext e ctx
  logWarn logger "Unexcpected error occurred"
  throwError err500 { errBody = "Internal server error" }
