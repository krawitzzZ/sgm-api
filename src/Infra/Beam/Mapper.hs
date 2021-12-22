module Infra.Beam.Mapper
  ( userEntityToDomain
  , eventEntityToDomain
  ) where

import           Domain.Event                             ( Event(..) )
import           Domain.User                              ( User(..) )
import           Infra.Beam.Schema.Latest                 ( EventEntity
                                                          , EventEntityT(..)
                                                          , PrimaryKey(UserEntityId)
                                                          , UserEntity
                                                          , UserEntityT(..)
                                                          )
import           RIO.Vector                               ( toList )


userEntityToDomain :: UserEntity -> User
userEntityToDomain UserEntity {..} = User { uId        = ueId
                                          , uUsername  = ueUsername
                                          , uPassword  = uePassword
                                          , uRoles     = toList ueRoles
                                          , uFirstName = ueFirstName
                                          , uLastName  = ueLastName
                                          }

eventEntityToDomain :: EventEntity -> Event
eventEntityToDomain EventEntity {..} =
  let (UserEntityId createdBy    ) = eeCreatedBy
      (UserEntityId lastUpdatedBy) = eeLastUpdatedBy
  in  Event { eId            = eeId
            , eTitle         = eeTitle
            , eDescription   = eeDescription
            , eCreatedBy     = createdBy
            , eLastUpdatedBy = lastUpdatedBy
            , eAttendees     = [] -- TODO add some attendees
            , eStart         = eeStart
            , eEnd           = eeEnd
            }
