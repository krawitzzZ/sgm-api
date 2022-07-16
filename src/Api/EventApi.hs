module Api.EventApi
  ( eventServer
  , EventApi
  ) where

import           Api.ApiVersion                                     ( ApiVersion(..) )
import           Api.Exception                                      ( ApiException(..)
                                                                    , throw401
                                                                    , tryCatchDefault
                                                                    )
import           Api.Mapper                                         ( eventToEventDto
                                                                    , newEventDtoToEventData
                                                                    , updateEventInfoDtoToEventData
                                                                    )
import           Api.Resources.Event                                ( EventDto
                                                                    , NewEventDto
                                                                    , UpdateEventInfoDto
                                                                    )
import           App.Event                                          ( attendEvent
                                                                    , createNewEvent
                                                                    , deleteEvent
                                                                    , findEventById
                                                                    , getEvents
                                                                    , unattendEvent
                                                                    , updateEventDetails
                                                                    )
import           Control.Exception.Safe                             ( MonadCatch )
import           Data.UUID                                          ( toText )
import           Domain.App.Class                                   ( AccessPolicyGuard
                                                                    , EventRepository
                                                                    , MonadLogger(..)
                                                                    )
import           Domain.App.Types                                   ( EventId(..)
                                                                    , UserId(..)
                                                                    )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Logger                                      ( LogContext
                                                                    , userIdKey
                                                                    )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<$>)
                                                                    , (<&>)
                                                                    , (<>)
                                                                    , (>>)
                                                                    , (>>>)
                                                                    , Text
                                                                    , const
                                                                    , map
                                                                    , return
                                                                    )
import           Servant                                            ( type (:<|>)(..)
                                                                    , type (:>)
                                                                    , Capture
                                                                    , Get
                                                                    , JSON
                                                                    , NoContent(..)
                                                                    , Post
                                                                    , Put
                                                                    , ReqBody
                                                                    , ServerT
                                                                    , StdMethod(..)
                                                                    , Verb
                                                                    )
import           Servant.Auth.Server                                ( Auth
                                                                    , AuthResult(..)
                                                                    )
import           Servant.Exception.Server                           ( Throws )
import           Utils                                              ( biconst )


type GetEvents = Get '[JSON] [EventDto]
type GetEvent = Capture "id" EventId :> Get '[JSON] EventDto
type CreateEvent = ReqBody '[JSON] NewEventDto :> Verb 'POST 201 '[JSON] EventDto
type UpdateEvent
  = Capture "id" EventId :> ReqBody '[JSON] UpdateEventInfoDto :> Put '[JSON] EventDto
type DeleteEvent = Capture "id" EventId :> Verb 'DELETE 204 '[JSON] NoContent
type AttendEvent = Capture "id" EventId :> "attend" :> Post '[JSON] NoContent
type UnattendEvent = Capture "id" EventId :> "unattend" :> Post '[JSON] NoContent

-- brittany-disable-next-binding
type EventApi auths = Throws ApiException :> Auth auths UserClaims :>
  (
    GetEvents :<|>
    GetEvent :<|>
    CreateEvent :<|>
    UpdateEvent :<|>
    DeleteEvent :<|>
    AttendEvent :<|>
    UnattendEvent
  )

eventServer
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => ApiVersion
  -> ServerT (EventApi auths) m
eventServer V1 = eventV1Server

eventV1Server
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => ServerT (EventApi auths) m
eventV1Server (Authenticated claims) =
  allEvents claims
    :<|> eventById claims
    :<|> newEvent claims
    :<|> updateEventInfo claims
    :<|> removeEvent claims
    :<|> attendAtEvent claims
    :<|> unattendAtEvent claims
eventV1Server _ =
  throw401
    :<|> const throw401
    :<|> const throw401
    :<|> biconst throw401
    :<|> const throw401
    :<|> const throw401
    :<|> const throw401


allEvents
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> m [EventDto]
allEvents claims =
  withContext (mkContext "allEvents")
    >>> withField (userIdKey, toText . unUserId . ucId $ claims)
    $   tryCatchDefault
    $   map eventToEventDto
    <$> getEvents claims

eventById
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> EventId
  -> m EventDto
eventById claims eventId =
  withContext (mkContext "eventById")
    >>> withFields (logFields claims eventId)
    $   tryCatchDefault
    $   findEventById claims eventId
    <&> eventToEventDto

newEvent
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> NewEventDto
  -> m EventDto
newEvent claims newEventDto =
  withContext (mkContext "newEvent")
    >>> withField (userIdKey, toText . unUserId . ucId $ claims)
    $   tryCatchDefault
    $   createNewEvent claims newEventData
    <&> eventToEventDto
  where newEventData = newEventDtoToEventData newEventDto claims

updateEventInfo
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> EventId
  -> UpdateEventInfoDto
  -> m EventDto
updateEventInfo claims eventId updateEventInfoDto =
  withContext (mkContext "updateEventInfo")
    >>> withFields (logFields claims eventId)
    $   tryCatchDefault
    $   updateEventDetails claims eventId updateEventData
    <&> eventToEventDto
  where updateEventData = updateEventInfoDtoToEventData updateEventInfoDto claims

removeEvent
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> EventId
  -> m NoContent
removeEvent claims eventId =
  withContext (mkContext "removeEvent")
    >>> withFields (logFields claims eventId)
    $   tryCatchDefault
    $   deleteEvent claims eventId
    >>  return NoContent

attendAtEvent
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> EventId
  -> m NoContent
attendAtEvent claims eventId =
  withContext (mkContext "attendAtEvent")
    >>> withFields (logFields claims eventId)
    $   tryCatchDefault
    $   attendEvent claims eventId
    >>  return NoContent

unattendAtEvent
  :: (AccessPolicyGuard m, EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> EventId
  -> m NoContent
unattendAtEvent claims eventId =
  withContext (mkContext "unattendAtEvent")
    >>> withFields (logFields claims eventId)
    $   tryCatchDefault
    $   unattendEvent claims eventId
    >>  return NoContent


logFields :: UserClaims -> EventId -> [(Text, Text)]
logFields claims (EventId eventId) =
  [(userIdKey, toText . unUserId . ucId $ claims), ("eventId", toText eventId)]

mkContext :: LogContext -> LogContext
mkContext = ("Api.EventApi->" <>)
