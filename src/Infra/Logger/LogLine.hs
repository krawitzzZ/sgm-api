module Infra.Logger.LogLine
  ( LogLine(..)
  , Severity(..)
  , fromDomain
  ) where

import           Data.Aeson                                         ( Options(..)
                                                                    , ToJSON(..)
                                                                    , defaultOptions
                                                                    , encode
                                                                    , genericToEncoding
                                                                    )
import           Data.Map.Strict                                    ( Map )
import qualified Domain.Logger                                     as DL
import           RIO                                                ( (.)
                                                                    , Bool(..)
                                                                    , Bounded
                                                                    , Enum
                                                                    , Eq
                                                                    , Generic
                                                                    , Maybe
                                                                    , Ord
                                                                    , Show
                                                                    , Text
                                                                    )
import           RIO.Time                                           ( UTCTime )
import           System.Log.FastLogger                              ( ToLogStr(..) )


data LogLine = LogLine
  { time     :: !UTCTime
  , severity :: !Severity
  , context  :: !Context
  , error    :: !(Maybe Text)
  , fields   :: !(Maybe (Map Text Text))
  , message  :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON LogLine where
  toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

instance ToLogStr LogLine where
  toLogStr = toLogStr . encode

type Context = Text

data Severity = Debug | Info | Warn | Error deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON Severity where

fromDomain :: DL.LogLevel -> Severity
fromDomain DL.Debug = Debug
fromDomain DL.Info  = Info
fromDomain DL.Warn  = Warn
fromDomain DL.Error = Error
