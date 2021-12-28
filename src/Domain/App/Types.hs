module Domain.App.Types
  ( UserId
  , EventId
  , CreatedBy
  , UpdatedBy
  ) where

import           Data.UUID                                          ( UUID )


type UserId = UUID

type EventId = UUID
type CreatedBy = UserId
type UpdatedBy = UserId
