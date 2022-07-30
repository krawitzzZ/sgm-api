module TestUtils
  ( mkUUID
  , mkUser
  , mkUsers
  , mkEvent
  , mkNewEventData
  , mkUpdateEventInfoData
  , mkJwtToken
  ) where

import           Control.Exception.Safe                             ( Exception
                                                                    , throwIO
                                                                    )
import qualified Data.Password.Argon2                              as P
import           Data.String.Conversions                            ( cs )
import           Data.UUID                                          ( UUID
                                                                    , fromString
                                                                    )
import           Data.UUID.V4                                       ( nextRandom )
import           Domain.App.Types                                   ( EventId(..)
                                                                    , UserId(..)
                                                                    )
import           Domain.Auth.Password                               ( PasswordHash(..) )
import           Domain.Auth.Role                                   ( Role(Participant) )
import           Domain.Event                                       ( Event(..) )
import           Domain.Event.EventData                             ( NewEventData(..)
                                                                    , UpdateEventInfoData(..)
                                                                    )
import           Domain.User                                        ( User(..) )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<&>)
                                                                    , (<>)
                                                                    , (>>=)
                                                                    , Eq
                                                                    , Generic
                                                                    , IO
                                                                    , Int
                                                                    , Maybe(..)
                                                                    , Show
                                                                    , Text
                                                                    , const
                                                                    , either
                                                                    , error
                                                                    , fromMaybe
                                                                    , mapM
                                                                    , return
                                                                    , show
                                                                    )
import           RIO.Text                                           ( unpack )
import           RIO.Time                                           ( UTCTime
                                                                    , utc
                                                                    , utcToLocalTime
                                                                    )
import           Servant.Auth.Client                                ( Token(..) )
import           Servant.Auth.Server                                ( JWTSettings
                                                                    , ToJWT
                                                                    , makeJWT
                                                                    )


mkUUID :: Text -> UUID
mkUUID t = fromMaybe (error "Invalid UUID string provided") $ fromString (unpack t)

mkUser :: UserId -> [Role] -> User
mkUser uid roles = User { uId        = uid
                        , uUsername  = "user"
                        , uPassword  = PasswordHash (P.PasswordHash "password")
                        , uRoles     = roles
                        , uFirstName = Nothing
                        , uLastName  = Nothing
                        }

mkUsers :: Int -> IO [User]
mkUsers n = mapM newUser [1 .. n]
 where
  newUser n' = do
    uid <- nextRandom
    return User { uId        = UserId uid
                , uUsername  = "user" <> cs (show n')
                , uPassword  = PasswordHash (P.PasswordHash "password")
                , uRoles     = [Participant]
                , uFirstName = Just $ "first name: " <> cs (show n')
                , uLastName  = Just $ "last name: " <> cs (show n')
                }

mkEvent :: EventId -> UserId -> UTCTime -> Event
mkEvent eid uid time = Event { eId            = eid
                             , eTitle         = "event"
                             , eDescription   = Nothing
                             , eCreatedBy     = uid
                             , eLastUpdatedBy = uid
                             , eAttendees     = []
                             , eStart         = utcToLocalTime utc time
                             , eEnd           = utcToLocalTime utc time
                             }

mkNewEventData :: Text -> UserId -> UTCTime -> NewEventData
mkNewEventData nedTitle uid time = NewEventData { nedTitle
                                                , nedDescription   = Nothing
                                                , nedCreatedBy     = uid
                                                , nedLastUpdatedBy = uid
                                                , nedStart         = utcToLocalTime utc time
                                                , nedEnd           = utcToLocalTime utc time
                                                }

mkUpdateEventInfoData :: Text -> UserId -> UTCTime -> UpdateEventInfoData
mkUpdateEventInfoData ueidTitle uid time = UpdateEventInfoData
  { ueidTitle
  , ueidDescription   = Nothing
  , ueidLastUpdatedBy = uid
  , ueidStart         = utcToLocalTime utc time
  , ueidEnd           = utcToLocalTime utc time
  }

mkJwtToken :: ToJWT a => a -> JWTSettings -> IO Token
mkJwtToken claims jwtConf =
  makeJWT claims jwtConf Nothing
    >>= either (const $ throwIO CreateTokenException) return
    <&> Token
    .   cs

data CreateTokenException = CreateTokenException
  deriving (Eq, Show, Generic)
instance Exception CreateTokenException
