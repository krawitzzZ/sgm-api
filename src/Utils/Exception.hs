module Utils.Exception
  ( mkTryCatch
  , mkTryCatches
  , mkTryCatchDefault
  ) where

import           Control.Exception.Safe                   ( Exception
                                                          , Handler(..)
                                                          , MonadCatch
                                                          , catches
                                                          )
import           RIO                                      ( (<>) )


mkTryCatch :: (MonadCatch m, Exception e) => Handler m a -> m a -> (e -> m a) -> m a
mkTryCatch fallbackHandler action handler = catches action [Handler handler, fallbackHandler]

mkTryCatches :: (MonadCatch m) => Handler m a -> m a -> [Handler m a] -> m a
mkTryCatches fallbackHandler action handlers = catches action (handlers <> [fallbackHandler])

mkTryCatchDefault :: (MonadCatch m) => [Handler m a] -> m a -> m a
mkTryCatchDefault defaultHandlers action = catches action defaultHandlers
