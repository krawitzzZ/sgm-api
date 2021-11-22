module Domain.Logger.LogLevel
  ( LogLevel(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          )
import           RIO                                      ( Bounded
                                                          , Enum
                                                          , Eq
                                                          , Generic
                                                          , Ord
                                                          , Read
                                                          , Show
                                                          )


data LogLevel = Debug | Info | Warn | Error deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
instance FromJSON LogLevel
instance ToJSON LogLevel where
