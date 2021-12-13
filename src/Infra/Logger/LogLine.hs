module Infra.Logger.LogLine
  ( LogLine(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , Options(..)
                                                          , ToJSON(..)
                                                          , defaultOptions
                                                          , encode
                                                          , genericToEncoding
                                                          )
import           Data.Map.Strict                          ( Map )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
import           Domain.Logger.LogMessage                 ( LogPath )
import           RIO                                      ( (.)
                                                          , Bool(..)
                                                          , Eq
                                                          , Generic
                                                          , Maybe(..)
                                                          , Show
                                                          , Text
                                                          )
import           RIO.Time                                 ( UTCTime )
import           System.Log.FastLogger                    ( ToLogStr(..) )


data LogLine = LogLine
  { time    :: !UTCTime
  , level   :: !LogLevel
  , message :: !Text
  , fields  :: !(Maybe (Map Text Text))
  , error   :: !(Maybe Text)
  , context :: !LogPath
  }
  deriving (Eq, Show, Generic)

instance FromJSON LogLine

instance ToJSON LogLine where
  toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

instance ToLogStr LogLine where
  toLogStr = toLogStr . encode
