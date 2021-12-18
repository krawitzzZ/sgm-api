module Domain.Logger
  ( Logger(..)
  , LogMessage(..)
  , LogLevel(..)
  , LogContext
  ) where

import           Data.Map.Strict                          ( Map )
import           Di.Core                                  ( Di )
import           RIO                                      ( Bounded
                                                          , Enum
                                                          , Eq
                                                          , Generic
                                                          , Maybe
                                                          , Ord
                                                          , Read
                                                          , Show
                                                          , Text
                                                          )


data Logger = Logger
  { loggerDi     :: !(Di LogLevel LogContext LogMessage)
  , loggerFields :: !(Map Text Text)
  , loggerError  :: !(Maybe Text)
  }

data LogMessage = LogMessage
  { lmMessage :: !Text
  , lmFields  :: !(Map Text Text)
  , lmError   :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type LogContext = Text

data LogLevel = Debug | Info | Warn | Error deriving (Eq, Ord, Show, Read, Enum, Bounded)
