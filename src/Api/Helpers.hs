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
import           Data.String.Conversions                  ( cs )
import           Domain.App                               ( AppM )
import           Infra.Logger                             ( logWarn )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , Text
                                                          , show
                                                          )
import           Servant                                  ( err500
                                                          , errBody
                                                          , throwError
                                                          )


tryCatch :: (Exception e) => Text -> (e -> AppM a) -> AppM a -> AppM a
tryCatch ctx handler action = catches action [Handler handler, catchInternalError ctx]

tryCatchAny :: Text -> AppM a -> AppM a
tryCatchAny ctx action = catches action [catchInternalError ctx]

tryCatches :: Text -> [Handler AppM a] -> AppM a -> AppM a
tryCatches ctx handlers action = catches action (handlers <> [catchInternalError ctx])

catchInternalError :: Text -> Handler AppM a
catchInternalError ctx = Handler $ \(SomeException e) -> do
  logWarn ctx (cs $ "Unexcpected error occurred: " <> show e)
  throwError err500 { errBody = "Internal server error" }
