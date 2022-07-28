module Domain.App.Types
  ( UserId(..)
  , EventId(..)
  ) where

import           Data.Aeson                                         ( FromJSON
                                                                    , ToJSON
                                                                    )
import           Data.UUID                                          ( UUID )
import           RIO                                                ( Eq
                                                                    , Generic
                                                                    , Show
                                                                    )
import           Web.HttpApiData                                    ( FromHttpApiData
                                                                    , ToHttpApiData
                                                                    )


newtype UserId = UserId { unUserId :: UUID }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

newtype EventId = EventId { unEventId :: UUID }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)
