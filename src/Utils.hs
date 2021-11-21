module Utils
  ( jsonOptions
  , readEnv
  ) where

import           Data.Aeson                               ( Options(..)
                                                          , defaultOptions
                                                          )
import           Data.String.Conversions                  ( cs )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (>>>)
                                                          , Foldable(length)
                                                          , MonadIO(..)
                                                          , Read
                                                          , String
                                                          , Text
                                                          , drop
                                                          , fromMaybe
                                                          , maybe
                                                          , readMaybe
                                                          , return
                                                          )
import           RIO.Char                                 ( toLower )
import           System.Environment                       ( lookupEnv )


jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions { fieldLabelModifier = dropPrefix >>> firstToLower }
 where
  firstToLower []         = []
  firstToLower (c : rest) = toLower c : rest
  dropPrefix = drop $ length prefix

readEnv :: (MonadIO m, Read a) => Text -> a -> m a
readEnv key defaultValue = do
  envValue <- liftIO $ lookupEnv $ cs key
  let result = maybe defaultValue (fromMaybe defaultValue . readMaybe) envValue
  return result
