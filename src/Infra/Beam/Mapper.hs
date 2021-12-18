module Infra.Beam.Mapper
  ( userEntityToDomain
  , eventEntityToDomain
  ) where

import           Domain.Event                             ( Event(..) )
import           Domain.User                              ( User(..) )
import           Infra.Beam.Schema.Latest                 ( EventEntity
                                                          , EventEntityT(..)
                                                          , UserEntity
                                                          , UserEntityT(..)
                                                          )


userEntityToDomain :: UserEntity -> User
userEntityToDomain (UserEntity uId _ _ name pass fname lname) = User uId name pass fname lname

eventEntityToDomain :: EventEntity -> User -> User -> Event
eventEntityToDomain (EventEntity uId _ _ title desc start end _ _) createdBy updatedBy =
  Event uId title desc createdBy updatedBy [] start end
