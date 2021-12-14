module Utils
  ( toText
  , jsonOptions
  , readEnvDefault
  , readEnvText
  , uuidFromText
  ) where

import           Data.Aeson                               ( Options(..)
                                                          , defaultOptions
                                                          )
import           Data.String.Conversions                  ( cs )
import           Data.UUID                                ( UUID
                                                          , fromText
                                                          )
import           Infra.Db.Schema.Types                    ( TextUUID )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<>)
                                                          , (>>>)
                                                          , Maybe(..)
                                                          , MonadIO(..)
                                                          , Read
                                                          , Show
                                                          , String
                                                          , Text
                                                          , drop
                                                          , error
                                                          , fromMaybe
                                                          , length
                                                          , maybe
                                                          , readMaybe
                                                          , return
                                                          , show
                                                          )
import           RIO.Char                                 ( toLower )
import           System.Environment                       ( lookupEnv )


toText :: (Show s) => s -> Text
toText = cs . show

uuidFromText :: TextUUID -> UUID
uuidFromText = fromMaybe (error "Failed to parse UUID") . fromText

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
