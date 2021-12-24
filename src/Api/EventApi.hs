module Api.EventApi
  ( eventServer
  , EventApi
  ) where

import           Api.ApiVersion                           ( ApiVersion(..) )
import           Api.Exception                            ( ApiException(..)
                                                          , throw401
                                                          , tryCatchDefault
                                                          )
import           Api.Mapper                               ( eventToEventDto
                                                          , newEventDtoToEventData
                                                          , updateEventInfoDtoToEventData
                                                          )
import           Api.Resources.Event                      ( EventDto
                                                          , NewEventDto
                                                          , UpdateEventInfoDto
                                                          )
import           App.Event                                ( createNewEvent
                                                          , deleteEvent
                                                          , findEventById
                                                          , getEvents
                                                          , updateEventDetails
                                                          )
import           Control.Exception.Safe                   ( MonadCatch )
import           Data.UUID                                ( UUID
                                                          , toText
                                                          )
import           Domain.App.Class                         ( EventRepository
                                                          , MonadLogger(..)
                                                          )
import           Domain.Auth.UserClaims                   ( UserClaims(..) )
import           Domain.Logger                            ( LogContext
                                                          , userIdKey
                                                          )
import           RIO                                      ( ($)
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
import           Servant                                  ( type (:<|>)(..)
                                                          , type (:>)
                                                          , Capture
                                                          , Get
                                                          , JSON
                                                          , NoContent(..)
                                                          , Put
                                                          , ReqBody
                                                          , ServerT
                                                          , StdMethod(..)
                                                          , Verb
                                                          )
import           Servant.Auth.Server                      ( Auth
                                                          , AuthResult(..)
                                                          )
import           Servant.Exception.Server                 ( Throws )
import           Utils                                    ( biconst )


type GetEvents = Get '[JSON] [EventDto]
type GetEvent = Capture "id" UUID :> Get '[JSON] EventDto
type CreateEvent = ReqBody '[JSON] NewEventDto :> Verb 'POST 201 '[JSON] EventDto
type UpdateEvent = Capture "id" UUID :> ReqBody '[JSON] UpdateEventInfoDto :> Put '[JSON] EventDto
type DeleteEvent = Capture "id" UUID :> Verb 'DELETE 204 '[JSON] NoContent

-- brittany-disable-next-binding
type EventApi auths = Throws ApiException :> Auth auths UserClaims :>
  (
    GetEvents :<|>
    GetEvent :<|>
    CreateEvent :<|>
    UpdateEvent :<|>
    DeleteEvent
  )

eventServer
  :: (EventRepository m, MonadCatch m, MonadLogger m) => ApiVersion -> ServerT (EventApi auths) m
eventServer V1 = eventV1Server

eventV1Server :: (EventRepository m, MonadCatch m, MonadLogger m) => ServerT (EventApi auths) m
eventV1Server (Authenticated u) =
  allEvents u :<|> eventById u :<|> newEvent u :<|> updateEventInfo u :<|> removeEvent u
eventV1Server _ =
  throw401 :<|> const throw401 :<|> const throw401 :<|> biconst throw401 :<|> const throw401


allEvents :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> m [EventDto]
allEvents me =
  withContext (mkContext "allEvents")
    >>> withField (userIdKey, toText . ucId $ me)
    $   tryCatchDefault
    $   map eventToEventDto
    <$> getEvents me

eventById :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> UUID -> m EventDto
eventById me eventId =
  withContext (mkContext "eventById")
    >>> withFields (logFields me eventId)
    $   tryCatchDefault
    $   findEventById me eventId
    <&> eventToEventDto

newEvent
  :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> NewEventDto -> m EventDto
newEvent me newEventDto =
  withContext (mkContext "newEvent")
    >>> withField (userIdKey, toText . ucId $ me)
    $   tryCatchDefault
    $   createNewEvent me newEventData
    <&> eventToEventDto
  where newEventData = newEventDtoToEventData newEventDto me

updateEventInfo
  :: (EventRepository m, MonadCatch m, MonadLogger m)
  => UserClaims
  -> UUID
  -> UpdateEventInfoDto
  -> m EventDto
updateEventInfo me eventId updateEventInfoDto =
  withContext (mkContext "updateEventInfo")
    >>> withFields (logFields me eventId)
    $   tryCatchDefault
    $   updateEventDetails me eventId updateEventData
    <&> eventToEventDto
  where updateEventData = updateEventInfoDtoToEventData updateEventInfoDto me

removeEvent
  :: (EventRepository m, MonadCatch m, MonadLogger m) => UserClaims -> UUID -> m NoContent
removeEvent me eventId =
  withContext (mkContext "removeEvent")
    >>> withFields (logFields me eventId)
    $   tryCatchDefault
    $   deleteEvent me eventId
    >>  return NoContent


logFields :: UserClaims -> UUID -> [(Text, Text)]
logFields me eventId = [(userIdKey, toText . ucId $ me), ("eventId", toText eventId)]

mkContext :: LogContext -> LogContext
mkContext = ("Api.EventApi->" <>)
