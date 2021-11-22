
module Infra.Logger.StdErr
  ( mkLogFunc
  ) where

import           Domain.Logger.LogMessage                 ( LogMessage )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , IO
                                                          , MonadIO
                                                          , liftIO
                                                          , return
                                                          )
import           System.Log.FastLogger                    ( ToLogStr(..)
                                                          , defaultBufSize
                                                          , newStderrLoggerSet
                                                          , pushLogStrLn
                                                          )


mkLogFunc :: MonadIO m => m (LogMessage -> IO ())
mkLogFunc = do
  stdErrLogger <- liftIO $ newStderrLoggerSet defaultBufSize
  return $ pushLogStrLn stdErrLogger . toLogStr
