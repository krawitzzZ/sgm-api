module Domain.Logger
  ( LogLevel(..)
  , LogMessage(..)
  , Logger(..)
  , HasLogger(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          , defaultOptions
                                                          , encode
                                                          , genericToEncoding
                                                          )
import           RIO                                      ( (.)
                                                          , Bounded
                                                          , Enum
                                                          , Eq
                                                          , Generic
                                                          , IO
                                                          , Ord
                                                          , Read
                                                          , Show
                                                          , Text
                                                          , id
                                                          )
import           RIO.Time                                 ( UTCTime )
import           System.Log.FastLogger                    ( ToLogStr(..) )


data LogLevel = Debug | Info | Warn | Error deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
instance FromJSON LogLevel
instance ToJSON LogLevel where

data LogMessage = LogMessage
  { time    :: !UTCTime
  , level   :: !LogLevel
  , message :: !Text
  , context :: !Text
  , version :: !Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions
instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

newtype Logger = Logger { logMsg :: LogMessage -> IO () }
class HasLogger env where
  {-# MINIMAL getLogger #-}
  getLogger  :: env -> Logger
  getLogFunc :: env -> LogMessage -> IO ()
  getLogFunc env = logMsg (getLogger env)
instance HasLogger Logger where
  getLogger = id
