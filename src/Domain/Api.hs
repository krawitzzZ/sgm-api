module Domain.Api
  ( ApiVersion(..)
  ) where

import           Data.Aeson                               ( FromJSON(..)
                                                          , ToJSON(..)
                                                          , defaultOptions
                                                          , genericToEncoding
                                                          )
import           Data.String.Conversions                  ( cs )
import           GHC.Read                                 ( readsPrec )
import           RIO                                      ( ($)
                                                          , (<>)
                                                          , Bounded
                                                          , Either(..)
                                                          , Enum
                                                          , Eq
                                                          , Generic
                                                          , Maybe(..)
                                                          , Ord
                                                          , Read
                                                          , Show(..)
                                                          , readMaybe
                                                          , show
                                                          )
import           Web.HttpApiData                          ( FromHttpApiData(..) )


data ApiVersion = V1
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show ApiVersion where
  show V1 = "v1"

instance Read ApiVersion where
  readsPrec _ "v1" = [(V1, "")]
  readsPrec _ _    = []

instance FromJSON ApiVersion

instance ToJSON ApiVersion where
  toEncoding = genericToEncoding defaultOptions

instance FromHttpApiData ApiVersion where
  parseUrlPiece version = case maybeVersion of
    Just v  -> Right v
    Nothing -> Left $ "Invalid api version: " <> version
    where maybeVersion = readMaybe $ cs version :: Maybe ApiVersion
