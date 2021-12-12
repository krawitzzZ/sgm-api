module Domain.Exception
  ( DomainException(..)
  ) where

import           RIO                                      ( Eq
                                                          , Exception(..)
                                                          , Generic
                                                          , Show(..)
                                                          , Text
                                                          , Typeable
                                                          )


data DomainException =
  NotFound Text |
  AccessRestricted Text
  deriving (Eq, Show, Generic, Typeable)
instance Exception DomainException
