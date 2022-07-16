{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.App.EventSpec
  ( spec
  ) where

import           App.Event                                          ( attendEvent
                                                                    , createNewEvent
                                                                    , deleteEvent
                                                                    , findEventById
                                                                    , getEvents
                                                                    , unattendEvent
                                                                    , updateEventDetails
                                                                    )
import           Control.Exception.Safe                             ( throwM )
import           Data.UUID.V4                                       ( nextRandom )
import qualified Domain.App.Class                                  as C
import           Domain.App.Types                                   ( EventId(..)
                                                                    , UserId(..)
                                                                    )
import           Domain.Auth.UserClaims                             ( UserClaims(..) )
import           Domain.Event                                       ( Event(..) )
import qualified Domain.Event                                      as EA
                                                                    ( Action(..) )
import           Domain.Event.EventData                             ( NewEventData
                                                                    , UpdateEventInfoData(..)
                                                                    )
import           Domain.Exception                                   ( DomainException(..) )
import           Domain.Policy                                      ( HasActionPolicy(..) )
import           RIO                                                ( ($)
                                                                    , (<$>)
                                                                    , (==)
                                                                    , IO
                                                                    , return
                                                                    , undefined
                                                                    )
import           RIO.Time                                           ( getCurrentTime )
import           Test.HMock                                         ( ExpectContext(..)
                                                                    , makeMockable
                                                                    , runMockT
                                                                    , (|->)
                                                                    , (|=>)
                                                                    )
import           Test.Hspec                                         ( Spec
                                                                    , describe
                                                                    , it
                                                                    , parallel
                                                                    , shouldReturn
                                                                    , shouldThrow
                                                                    )
import           Test.Predicates                                    ( anything
                                                                    , eq
                                                                    , typed
                                                                    )
import           TestConstants                                      ( participantClaims
                                                                    , uuid1
                                                                    , uuid2
                                                                    )
import           TestUtils                                          ( mkEvent
                                                                    , mkNewEventData
                                                                    , mkUpdateEventInfoData
                                                                    )


makeMockable [t|C.EventRepository|]
makeMockable [t|C.AccessPolicyGuard|]

spec :: Spec
spec = parallel $ do
  describe "getEvents" $ do
    it "should successfully return list of events" $ do
      events <- someEvents
      getEventsSuccess events `shouldReturn` events
    it "should propagate AccessPolicyViolation exception if thrown" $ do
      getEventsPolicyError `shouldThrow` (== AccessPolicyViolation)
    it "should propagate InternalError exception if thrown" $ do
      getEventsInternalError `shouldThrow` (== InternalError "oops")

  describe "createNewEvent" $ do
    it "should successfully create an event" $ do
      eventData  <- mkNewEventData "test" (UserId uuid2) <$> getCurrentTime
      [event, _] <- someEvents
      createEventSuccess event eventData `shouldReturn` event
    it "should propagate AccessPolicyViolation exception if thrown" $ do
      createEventPolicyError `shouldThrow` (== AccessPolicyViolation)
    it "should propagate InternalError exception if thrown" $ do
      eventData <- mkNewEventData "test" (UserId uuid2) <$> getCurrentTime
      createEventInternalError eventData `shouldThrow` (== InternalError "not today")

  describe "findEventById" $ do
    it "should successfully return an event" $ do
      [event, _] <- someEvents
      getEventByIdSuccess event `shouldReturn` event
    it "should propagate AccessPolicyViolation exception if thrown" $ do
      getEventByIdPolicyError `shouldThrow` (== AccessPolicyViolation)
    it "should propagate InternalError exception if thrown" $ do
      getEventByIdInternalError `shouldThrow` (== InternalError "nope")

  describe "updateEventDetails" $ do
    it "should successfully update an event" $ do
      updateData <- mkUpdateEventInfoData "test" (UserId uuid2) <$> getCurrentTime
      [event, _] <- someEvents
      updateEventSuccess event updateData `shouldReturn` event
    it "should propagate NotFound exception if event does not exist" $ do
      updateData <- mkUpdateEventInfoData "test" (UserId uuid2) <$> getCurrentTime
      updateEventNotFound updateData `shouldThrow` (== NotFound "not found")
    it "should propagate AccessPolicyViolation exception if thrown" $ do
      updateData <- mkUpdateEventInfoData "test" (UserId uuid2) <$> getCurrentTime
      [event, _] <- someEvents
      updateEventPolicyError event updateData `shouldThrow` (== AccessPolicyViolation)
    it "should propagate InternalError exception if thrown" $ do
      updateData <- mkUpdateEventInfoData "test" (UserId uuid2) <$> getCurrentTime
      updateEventInternalError updateData `shouldThrow` (== InternalError "nope")

  describe "deleteEvent" $ do
    it "should successfully delete an event" $ do
      deleteEventSuccess `shouldReturn` ()
    it "should propagate NotFound exception if event does not exist" $ do
      deleteEventNotFound `shouldThrow` (== NotFound "not found")
    it "should propagate AccessPolicyViolation exception if thrown" $ do
      deleteEventPolicyError `shouldThrow` (== AccessPolicyViolation)
    it "should propagate InternalError exception if thrown" $ do
      deleteEventInternalError `shouldThrow` (== InternalError "nope")

  describe "attendEvent" $ do
    it "should successfully add an attendee to event" $ do
      attendEventSuccess `shouldReturn` ()
    it "should propagate NotFound exception if event does not exist" $ do
      attendEventNotFound `shouldThrow` (== NotFound "not found")
    it "should propagate AccessPolicyViolation exception if thrown" $ do
      attendEventPolicyError `shouldThrow` (== AccessPolicyViolation)
    it "should propagate InternalError exception if thrown" $ do
      attendEventInternalError `shouldThrow` (== InternalError "nope")

  describe "unattendEvent" $ do
    it "should successfully remove attendee from event" $ do
      unattendEventSuccess `shouldReturn` ()
    it "should propagate NotFound exception if event does not exist" $ do
      unattendEventNotFound `shouldThrow` (== NotFound "not found")
    it "should propagate AccessPolicyViolation exception if thrown" $ do
      unattendEventPolicyError `shouldThrow` (== AccessPolicyViolation)
    it "should propagate InternalError exception if thrown" $ do
      unattendEventInternalError `shouldThrow` (== InternalError "nope")


getEventsSuccess :: [Event] -> IO [Event]
getEventsSuccess events = do
  runMockT $ do
    expect $ CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.GetAllEvents)) |-> ()
    expect $ GetAllEvents |-> events
    getEvents participantClaims

getEventsPolicyError :: IO [Event]
getEventsPolicyError = runMockT $ do
  expect
    $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.GetAllEvents))
    |=> \_ -> throwM AccessPolicyViolation
  getEvents participantClaims

getEventsInternalError :: IO [Event]
getEventsInternalError = runMockT $ do
  expect $ CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.GetAllEvents)) |-> ()
  expect $ GetAllEvents |=> \_ -> throwM (InternalError "oops")
  getEvents participantClaims

createEventSuccess :: Event -> NewEventData -> IO Event
createEventSuccess event eData = runMockT $ do
  expect $ CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.CreateEvent)) |-> ()
  expect $ CreateEvent_ (eq eData) |-> event
  createNewEvent participantClaims eData

createEventPolicyError :: IO Event
createEventPolicyError = runMockT $ do
  expect
    $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.CreateEvent))
    |=> \_ -> throwM AccessPolicyViolation
  createNewEvent participantClaims undefined

createEventInternalError :: NewEventData -> IO Event
createEventInternalError eData = runMockT $ do
  expect $ CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.CreateEvent)) |-> ()
  expect $ CreateEvent_ (eq eData) |=> \_ -> throwM (InternalError "not today")
  createNewEvent participantClaims eData

getEventByIdSuccess :: Event -> IO Event
getEventByIdSuccess event = runMockT $ do
  expect $ CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.GetEvent)) |-> ()
  expect $ GetEventById_ (eq (EventId uuid1)) |-> event
  findEventById participantClaims (EventId uuid1)

getEventByIdPolicyError :: IO Event
getEventByIdPolicyError = runMockT $ do
  expect $ CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.GetEvent)) |=> \_ ->
    throwM AccessPolicyViolation
  findEventById participantClaims (EventId uuid1)

getEventByIdInternalError :: IO Event
getEventByIdInternalError = runMockT $ do
  expect $ CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq EA.GetEvent)) |-> ()
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (InternalError "nope")
  findEventById participantClaims (EventId uuid1)

updateEventSuccess :: Event -> UpdateEventInfoData -> IO Event
updateEventSuccess event updateData = runMockT $ do
  let eid = eId event
  let uid = eCreatedBy event
  expect $ GetEventById_ (eq eid) |-> event
  expect
    $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq (EA.UpdateEventInfo uid)))
    |-> ()
  expect $ SaveEvent_ anything |-> event
  updateEventDetails participantClaims eid updateData

updateEventNotFound :: UpdateEventInfoData -> IO Event
updateEventNotFound updateData = runMockT $ do
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (NotFound "not found")
  updateEventDetails participantClaims (EventId uuid1) updateData

updateEventPolicyError :: Event -> UpdateEventInfoData -> IO Event
updateEventPolicyError event updateData = runMockT $ do
  let eid = eId event
  let uid = eCreatedBy event
  expect $ GetEventById_ (eq eid) |-> event
  expect
    $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq (EA.UpdateEventInfo uid)))
    |=> \_ -> throwM AccessPolicyViolation
  updateEventDetails participantClaims eid updateData

updateEventInternalError :: UpdateEventInfoData -> IO Event
updateEventInternalError updateData = runMockT $ do
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (InternalError "nope")
  updateEventDetails participantClaims (EventId uuid1) updateData

deleteEventSuccess :: IO ()
deleteEventSuccess = do
  [event, _] <- someEvents
  let eid = eId event
  let uid = eCreatedBy event
  runMockT $ do
    expect $ GetEventById_ (eq eid) |-> event
    expect
      $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq (EA.DeleteEvent uid)))
      |-> ()
    expect $ DeleteEvent_ (eq eid) |-> ()
    deleteEvent participantClaims eid

deleteEventNotFound :: IO ()
deleteEventNotFound = runMockT $ do
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (NotFound "not found")
  deleteEvent participantClaims (EventId uuid1)

deleteEventPolicyError :: IO ()
deleteEventPolicyError = do
  [event, _] <- someEvents
  runMockT $ do
    let eid = eId event
    let uid = eCreatedBy event
    expect $ GetEventById_ (eq eid) |-> event
    expect
      $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq (EA.DeleteEvent uid)))
      |=> \_ -> throwM AccessPolicyViolation
    deleteEvent participantClaims eid

deleteEventInternalError :: IO ()
deleteEventInternalError = runMockT $ do
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (InternalError "nope")
  deleteEvent participantClaims (EventId uuid1)

attendEventSuccess :: IO ()
attendEventSuccess = do
  [event, _] <- someEvents
  let eid = eId event
  let uid = ucId participantClaims
  runMockT $ do
    expect $ GetEventById_ (eq eid) |-> event
    expect
      $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq (EA.AttendEvent uid)))
      |-> ()
    expect $ AttendEvent_ (eq event) (eq uid) |-> ()
    attendEvent participantClaims eid

attendEventNotFound :: IO ()
attendEventNotFound = runMockT $ do
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (NotFound "not found")
  attendEvent participantClaims (EventId uuid1)

attendEventPolicyError :: IO ()
attendEventPolicyError = do
  [event, _] <- someEvents
  runMockT $ do
    let eid = eId event
    let uid = ucId participantClaims
    expect $ GetEventById_ (eq eid) |-> event
    expect
      $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq (EA.AttendEvent uid)))
      |=> \_ -> throwM AccessPolicyViolation
    attendEvent participantClaims eid

attendEventInternalError :: IO ()
attendEventInternalError = runMockT $ do
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (InternalError "nope")
  attendEvent participantClaims (EventId uuid1)

unattendEventSuccess :: IO ()
unattendEventSuccess = do
  [event, _] <- someEvents
  let eid = eId event
  let uid = ucId participantClaims
  runMockT $ do
    expect $ GetEventById_ (eq eid) |-> event
    expect
      $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq (EA.UnattendEvent uid)))
      |-> ()
    expect $ UnattendEvent_ (eq event) (eq uid) |-> ()
    unattendEvent participantClaims eid

unattendEventNotFound :: IO ()
unattendEventNotFound = runMockT $ do
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (NotFound "not found")
  unattendEvent participantClaims (EventId uuid1)

unattendEventPolicyError :: IO ()
unattendEventPolicyError = do
  [event, _] <- someEvents
  runMockT $ do
    let eid = eId event
    let uid = ucId participantClaims
    expect $ GetEventById_ (eq eid) |-> event
    expect
      $   CheckPolicy_ (eq participantClaims) (typed @(Action Event) (eq (EA.UnattendEvent uid)))
      |=> \_ -> throwM AccessPolicyViolation
    unattendEvent participantClaims eid

unattendEventInternalError :: IO ()
unattendEventInternalError = runMockT $ do
  expect $ GetEventById_ (eq (EventId uuid1)) |=> \_ -> throwM (InternalError "nope")
  unattendEvent participantClaims (EventId uuid1)

someEvents :: IO [Event]
someEvents = do
  eid1 <- nextRandom
  uid1 <- nextRandom
  eid2 <- nextRandom
  uid2 <- nextRandom
  now  <- getCurrentTime
  return [mkEvent (EventId eid1) (UserId uid1) now, mkEvent (EventId eid2) (UserId uid2) now]
