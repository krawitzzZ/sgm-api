module Domain.Logger.LogMessage
  ( LogMessage(..)
  , LogPath
  ) where

import           Data.Map.Strict                          ( Map )
import           RIO                                      ( Eq
                                                          , Generic
                                                          , Maybe(..)
                                                          , Show
                                                          , Text
                                                          )


type LogPath = Text

data LogMessage = LogMessage
  { lmMessage :: !Text
  , lmFields  :: !(Map Text Text)
  , lmError   :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)
