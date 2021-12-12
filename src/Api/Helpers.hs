module Api.Helpers
  ( tryCatch
  , tryCatches
  , tryCatchAny
  ) where

import           Api.Exception                            ( ApiException(..) )
import           Control.Exception.Safe                   ( Exception
                                                          , Handler(..)
                                                          , MonadCatch
                                                          , MonadThrow
                                                          , SomeException(..)
                                                          , catches
                                                          , throwM
                                                          )
import           Domain.Logger.Class                      ( MonadLogger(..) )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          )


tryCatch :: (MonadLogger m, MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
tryCatch action handler = catches action [Handler handler, catchSomeException]

tryCatches :: (MonadLogger m, MonadCatch m) => m a -> [Handler m a] -> m a
tryCatches action handlers = catches action (handlers <> [catchSomeException])

tryCatchAny :: (MonadLogger m, MonadCatch m) => m a -> m a
tryCatchAny action = catches action [catchSomeException]

catchSomeException :: (MonadLogger m, MonadThrow m) => Handler m a
catchSomeException = Handler $ \(SomeException e) -> do
  withError e $ logWarn "Unexpected exception occurred"
  throwM InternalError
