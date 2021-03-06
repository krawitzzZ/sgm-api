module Domain.Logger
  ( Logger(..)
  , LogMessage(..)
  , LogLevel(..)
  , LogContext
  , userIdKey
  ) where

import           Data.Map.Strict                                    ( Map )
import           Di.Core                                            ( Di )
import           RIO                                                ( Bounded
                                                                    , Enum
                                                                    , Eq
                                                                    , Maybe
                                                                    , Ord
                                                                    , Read
                                                                    , Show
                                                                    , Text
                                                                    )


data Logger = Logger
  { lDi     :: !(Di LogLevel LogContext LogMessage)
  , lFields :: !(Map Text Text)
  , lError  :: !(Maybe Text)
  }

data LogMessage = LogMessage
  { lmMessage :: !Text
  , lmFields  :: !(Map Text Text)
  , lmError   :: !(Maybe Text)
  }

type LogContext = Text

data LogLevel = Debug | Info | Warn | Error deriving (Eq, Ord, Show, Read, Enum, Bounded)

userIdKey :: Text
userIdKey = "userId"
