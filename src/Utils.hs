module Utils
  ( toUTC
  , jsonOptions
  , readEnvDefault
  , readEnvText
  ) where

import           Data.Aeson                               ( Options(..)
                                                          , defaultOptions
                                                          )
import           Data.String.Conversions                  ( cs )
import           Data.Time.LocalTime.TimeZone.Detect      ( TimeZoneName
                                                          , timeInTimeZoneToUTC
                                                          )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<>)
                                                          , (>>>)
                                                          , Maybe(..)
                                                          , MonadIO(..)
                                                          , Read
                                                          , String
                                                          , Text
                                                          , drop
                                                          , error
                                                          , fromMaybe
                                                          , length
                                                          , maybe
                                                          , readMaybe
                                                          , return
                                                          )
import           RIO.Char                                 ( toLower )
import           RIO.Time                                 ( LocalTime
                                                          , UTCTime
                                                          )
import           System.Environment                       ( lookupEnv )


toUTC :: MonadIO m => LocalTime -> m UTCTime
toUTC time = liftIO $ timeInTimeZoneToUTC sgmTZ time

sgmTZ :: TimeZoneName
sgmTZ = "Europe/Berlin"

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions { fieldLabelModifier = dropPrefix >>> firstToLower }
 where
  firstToLower []         = []
  firstToLower (c : rest) = toLower c : rest
  dropPrefix = drop $ length prefix

readEnvDefault :: (MonadIO m, Read a) => Text -> a -> m a
readEnvDefault key defaultValue = do
  envValue <- liftIO $ lookupEnv $ cs key
  return $ maybe defaultValue (fromMaybe defaultValue . readMaybe) envValue

readEnvText :: (MonadIO m) => Text -> m Text
readEnvText key = do
  envValue <- liftIO $ lookupEnv $ cs key
  case envValue of
    Nothing    -> error $ "Environment variable " <> cs key <> " is required"
    Just value -> return $ cs value
