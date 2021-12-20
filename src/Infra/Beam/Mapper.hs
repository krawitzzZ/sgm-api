module Infra.Beam.Mapper
  ( userEntityToDomain
  , eventEntityToDomain
  ) where

import           Data.UUID                                ( UUID )
import           Domain.Event                             ( Event(..) )
import           Domain.User                              ( User(..) )
import           Infra.Beam.Schema.Latest                 ( EventEntity
                                                          , EventEntityT(..)
                                                          , UserEntity
                                                          , UserEntityT(..)
                                                          )


userEntityToDomain :: UserEntity -> User
userEntityToDomain UserEntity { ueId, ueUsername, uePassword, ueFirstName, ueLastName } = User
  { uId        = ueId
  , uUsername  = ueUsername
  , uPassword  = uePassword
  , uFirstName = ueFirstName
  , uLastName  = ueLastName
  }

eventEntityToDomain :: EventEntity -> UUID -> UUID -> Event
eventEntityToDomain EventEntity { eeId, eeTitle, eeDescription, eeStart, eeEnd } createdBy updatedBy
  = Event { eId            = eeId
          , eTitle         = eeTitle
          , eDescription   = eeDescription
          , eCreatedBy     = createdBy
          , eLastUpdatedBy = updatedBy
          , eAttendees     = [] -- TODO add some attendees
          , eStart         = eeStart
          , eEnd           = eeEnd
          }
