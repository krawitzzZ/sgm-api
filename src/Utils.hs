module Utils
  ( toUTC
  , toMaybe
  , jsonOptions
  , readEnvDefault
  , readEnvText
  , anyElem
  , biconst
  , triconst
  , tetconst
  ) where

import           Data.Aeson                                         ( Options(..)
                                                                    , defaultOptions
                                                                    )
import           Data.String.Conversions                            ( cs )
import           Data.Time.LocalTime.TimeZone.Detect                ( TimeZoneName
                                                                    , timeInTimeZoneToUTC
                                                                    )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<>)
                                                                    , (>>>)
                                                                    , Bool(..)
                                                                    , Eq
                                                                    , Maybe(..)
                                                                    , MonadIO(..)
                                                                    , Read
                                                                    , String
                                                                    , Text
                                                                    , any
                                                                    , const
                                                                    , drop
                                                                    , elem
                                                                    , error
                                                                    , fromMaybe
                                                                    , length
                                                                    , maybe
                                                                    , readMaybe
                                                                    , return
                                                                    )
import           RIO.Char                                           ( toLower )
import           RIO.Time                                           ( LocalTime
                                                                    , UTCTime
                                                                    )
import           System.Environment                                 ( lookupEnv )

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

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

anyElem :: (Eq a) => [a] -> [a] -> Bool
anyElem needles haystack = any (`elem` haystack) needles

biconst :: a -> b -> c -> a
biconst = const . const

triconst :: a -> b -> c -> d -> a
triconst = const . const . const

tetconst :: a -> b -> c -> d -> e -> a
tetconst = const . const . const . const
