module Domain.Exception
  ( DomainException(..)
  , NotFound(..)
  , AccessRestricted(..)
  ) where

import           RIO                                      ( (.)
                                                          , Eq
                                                          , Exception(..)
                                                          , Maybe(..)
                                                          , Show(..)
                                                          , Text
                                                          , Typeable
                                                          )


data DomainException = DNotFound NotFound | DAccessRestricted AccessRestricted
  deriving (Eq, Typeable)
instance Exception DomainException
instance Show DomainException where
  show (DNotFound         e) = show e
  show (DAccessRestricted e) = show e

newtype NotFound = NotFound Text
  deriving (Eq, Show, Typeable)
instance Exception NotFound where
  toException = toException . DNotFound
  fromException se = case fromException se of
    Just (DNotFound e) -> Just e
    _                  -> Nothing

newtype AccessRestricted = AccessRestricted Text
  deriving (Eq, Show, Typeable)
instance Exception AccessRestricted where
  toException = toException . DAccessRestricted
  fromException se = case fromException se of
    Just (DAccessRestricted e) -> Just e
    _                          -> Nothing
