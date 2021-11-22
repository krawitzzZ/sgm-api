module Domain.Logger.LogMessage
  ( LogMessage(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , Options(..)
                                                          , ToJSON(..)
                                                          , defaultOptions
                                                          , encode
                                                          , genericToEncoding
                                                          )
import           Domain.Logger.LogLevel                   ( LogLevel(..) )
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


data LogMessage = LogMessage
  { time    :: !UTCTime
  , level   :: !LogLevel
  , message :: !Text
  -- , fields :: !Text
  , error   :: !(Maybe Text)
  , context :: !Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }
instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode
