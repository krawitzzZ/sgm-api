module Infra.Beam.Mapper
  ( userEntityToDomain
  , eventEntityToDomain
  ) where

import           Domain.App.Types                                   ( EventId(..)
                                                                    , UserId(..)
                                                                    )
import           Domain.Event                                       ( Event(..) )
import           Domain.User                                        ( User(..) )
import           Infra.Beam.Schema.Latest                           ( EventEntity
                                                                    , EventEntityT(..)
                                                                    , PrimaryKey(..)
                                                                    , UserEntity
                                                                    , UserEntityId
                                                                    , UserEntityT(..)
                                                                    )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , map
                                                                    )
import           RIO.Vector                                         ( toList )


userEntityToDomain :: UserEntity -> User
userEntityToDomain UserEntity {..} = User { uId        = UserId ueId
                                          , uUsername  = ueUsername
                                          , uPassword  = uePassword
                                          , uRoles     = toList ueRoles
                                          , uFirstName = ueFirstName
                                          , uLastName  = ueLastName
                                          }

eventEntityToDomain :: EventEntity -> [UserEntityId] -> Event
eventEntityToDomain EventEntity {..} attendeesIds = Event
  { eId            = EventId eeId
  , eTitle         = eeTitle
  , eDescription   = eeDescription
  , eCreatedBy     = UserId $ userEntityId eeCreatedBy
  , eLastUpdatedBy = UserId $ userEntityId eeLastUpdatedBy
  , eAttendees     = map (UserId . userEntityId) attendeesIds
  , eStart         = eeStart
  , eEnd           = eeEnd
  }
