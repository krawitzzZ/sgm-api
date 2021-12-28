module Api.Resources.Event
  ( EventDto(..)
  , NewEventDto(..)
  , UpdateEventInfoDto(..)
  ) where

import           Data.Aeson                                         ( FromJSON(..)
                                                                    , ToJSON(..)
                                                                    , genericParseJSON
                                                                    , genericToJSON
                                                                    )
import           Data.Validity                                      ( Validity(..)
                                                                    , declare
                                                                    )
import           Data.Validity.Aeson                                ( parseJSONValid )
import           Data.Validity.Text                                 ( )
import           Data.Validity.Time.LocalTime                       ( )
import           Data.Validity.UUID                                 ( )
import           Domain.App.Types                                   ( EventId
                                                                    , UserId
                                                                    )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<=)
                                                                    , (>)
                                                                    , Bool(..)
                                                                    , Generic
                                                                    , Maybe(..)
                                                                    , Text
                                                                    , maybe
                                                                    , mconcat
                                                                    )
import           RIO.Text                                           ( length )
import           RIO.Time                                           ( LocalTime )
import           Utils                                              ( jsonOptions )


data EventDto = EventDto
  { eDtoId            :: !EventId
  , eDtoTitle         :: !Text
  , eDtoDescription   :: !(Maybe Text)
  , eDtoCreatedBy     :: !UserId
  , eDtoLastUpdatedBy :: !UserId
  , eDtoAttendees     :: ![UserId]
  , eDtoStart         :: !LocalTime
  , eDtoEnd           :: !LocalTime
  }
  deriving Generic

instance ToJSON EventDto where
  toJSON = genericToJSON (jsonOptions "eDto")

data NewEventDto = NewEventDto
  { neDtoTitle       :: !Text
  , neDtoDescription :: !(Maybe Text)
  , neDtoStart       :: !LocalTime -- TODO check if LocalTime validations works
  , neDtoEnd         :: !LocalTime
  }
  deriving Generic

instance FromJSON NewEventDto where
  parseJSON v = parseJSONValid $ genericParseJSON (jsonOptions "neDto") v

instance Validity NewEventDto where
  validate NewEventDto {..} = mconcat
    [ declare "Title is at least 5 characters long"    (length neDtoTitle > 5)
    , declare "Title is not longer than 30 characters" (length neDtoTitle <= 30)
    , declare "Description is at least 10 characters long"
              (maybe True ((> 10) . length) neDtoDescription)
    , declare "Description is not longer than 300 characters"
              (maybe True ((<= 300) . length) neDtoDescription)
    , declare "End is after start" (neDtoEnd > neDtoStart)
    ]

data UpdateEventInfoDto = UpdateEventInfoDto
  { ueiDtoTitle       :: !Text
  , ueiDtoDescription :: !(Maybe Text)
  , ueiDtoStart       :: !LocalTime
  , ueiDtoEnd         :: !LocalTime
  }
  deriving Generic

instance FromJSON UpdateEventInfoDto where
  parseJSON v = parseJSONValid $ genericParseJSON (jsonOptions "ueiDto") v

instance Validity UpdateEventInfoDto where
  validate UpdateEventInfoDto {..} = mconcat
    [ declare "Title is at least 5 characters long"    (length ueiDtoTitle > 5)
    , declare "Title is not longer than 30 characters" (length ueiDtoTitle <= 30)
    , declare "Description is at least 10 characters long"
              (maybe True ((> 10) . length) ueiDtoDescription)
    , declare "Description is not longer than 300 characters"
              (maybe True ((<= 300) . length) ueiDtoDescription)
    , declare "End is after start" (ueiDtoEnd > ueiDtoStart)
    ]
