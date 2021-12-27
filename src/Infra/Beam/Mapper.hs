module Infra.Beam.Mapper
  ( userEntityToDomain
  , eventEntityToDomain
  ) where

import           Domain.Event                                       ( Event(..) )
import           Domain.User                                        ( User(..) )
import           Infra.Beam.Schema.Latest                           ( EventEntity
                                                                    , EventEntityT(..)
                                                                    , PrimaryKey(..)
                                                                    , UserEntity
                                                                    , UserEntityId
                                                                    , UserEntityT(..)
                                                                    )
import           RIO                                                ( map )
import           RIO.Vector                                         ( toList )


userEntityToDomain :: UserEntity -> User
userEntityToDomain UserEntity {..} = User { uId        = ueId
                                          , uUsername  = ueUsername
                                          , uPassword  = uePassword
                                          , uRoles     = toList ueRoles
                                          , uFirstName = ueFirstName
                                          , uLastName  = ueLastName
                                          }

eventEntityToDomain :: EventEntity -> [UserEntityId] -> Event
eventEntityToDomain EventEntity {..} attendeesIds = Event
  { eId            = eeId
  , eTitle         = eeTitle
  , eDescription   = eeDescription
  , eCreatedBy     = userEntityId eeCreatedBy
  , eLastUpdatedBy = userEntityId eeLastUpdatedBy
  , eAttendees     = map userEntityId attendeesIds
  , eStart         = eeStart
  , eEnd           = eeEnd
  }
