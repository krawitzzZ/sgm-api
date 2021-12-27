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
                                                                    , updateEventDetails
                                                                    )
import           Control.Exception.Safe                             ( MonadCatch )
import           Data.UUID                                          ( UUID
                                                                    , toText
                                                                    )
import           Domain.App.Class                                   ( EventRepository
                                                                    , MonadLogger(..)
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
type GetEvent = Capture "id" UUID :> Get '[JSON] EventDto
type CreateEvent = ReqBody '[JSON] NewEventDto :> Verb 'POST 201 '[JSON] EventDto
type UpdateEvent = Capture "id" UUID :> ReqBody '[JSON] UpdateEventInfoDto :> Put '[JSON] EventDto
type DeleteEvent = Capture "id" UUID :> Verb 'DELETE 204 '[JSON] NoContent
type AttendEvent = Capture "id" UUID :> "attend" :> Post '[JSON] NoContent
-- type UnattendEvent = Capture "id" UUID :> "unattend" :> Post '[JSON] NoContent -- TODO unattend

-- brittany-disable-next-binding
type EventApi auths = Throws ApiException :> Auth auths UserClaims :>
  (
    GetEvents :<|>
    GetEvent :<|>
    CreateEvent :<|>
    UpdateEvent :<|>
    DeleteEvent :<|>
    AttendEvent
  )

eventServer
  :: (EventRepository m, MonadCatch m, MonadLogger m) => ApiVersion -> ServerT (EventApi auths) m
eventServer V1 = eventV1Server

eventV1Server :: (EventRepository m, MonadCatch m, MonadLogger m) => ServerT (EventApi auths) m
eventV1Server (Authenticated claims) =
  allEvents claims
    :<|> eventById claims
    :<|> newEvent claims
    :<|> updateEventInfo claims
    :<|> removeEvent claims
    :<|> attendAtEvent claims
eventV1Server _ =
  throw401
    :<|> const throw401
    :<|> const throw401
    :<|> biconst throw401
    :<|> const throw401
    :<|> const throw401


allEvents :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> m [EventDto]
allEvents claims =
  withContext (mkContext "allEvents")
    >>> withField (userIdKey, toText . ucId $ claims)
    $   tryCatchDefault
    $   map eventToEventDto
    <$> getEvents claims

eventById :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> UUID -> m EventDto
eventById claims eventId =
  withContext (mkContext "eventById")
    >>> withFields (logFields claims eventId)
    $   tryCatchDefault
    $   findEventById claims eventId
    <&> eventToEventDto

newEvent
  :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> NewEventDto -> m EventDto
newEvent claims newEventDto =
  withContext (mkContext "newEvent")
    >>> withField (userIdKey, toText . ucId $ claims)
    $   tryCatchDefault
    $   createNewEvent claims newEventData
    <&> eventToEventDto
  where newEventData = newEventDtoToEventData newEventDto claims

updateEventInfo
  :: (EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> UUID
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
  :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> UUID -> m NoContent
removeEvent claims eventId =
  withContext (mkContext "removeEvent")
    >>> withFields (logFields claims eventId)
    $   tryCatchDefault
    $   deleteEvent claims eventId
    >>  return NoContent

attendAtEvent
  :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> UUID -> m NoContent
attendAtEvent claims eventId =
  withContext (mkContext "attendAtEvent")
    >>> withFields (logFields claims eventId)
    $   tryCatchDefault
    $   attendEvent claims eventId
    >>  return NoContent


logFields :: UserClaims -> UUID -> [(Text, Text)]
logFields claims eventId = [(userIdKey, toText . ucId $ claims), ("eventId", toText eventId)]

mkContext :: LogContext -> LogContext
mkContext = ("Api.EventApi->" <>)
