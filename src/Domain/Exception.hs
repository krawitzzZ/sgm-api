module Domain.Exception
  ( DomainException(..)
  ) where

import           Data.String.Conversions                  ( cs )
import           RIO                                      ( (<>)
                                                          , Eq
                                                          , Exception
                                                          , Show(..)
                                                          , Text
                                                          )


data DomainException =
  InternalError Text |
  NotFound Text |
  UserNameAlreadyExists Text |
  InvalidPassword Text |
  CreateJwtException Text |
  AccessRestricted
  deriving (Eq)

instance Show DomainException where
  show (InternalError         msg) = "InternalError => " <> cs msg
  show (NotFound              msg) = "NotFound => " <> cs msg
  show (UserNameAlreadyExists msg) = "UserNameAlreadyExists => " <> cs msg
  show (InvalidPassword       msg) = "InvalidPassword => " <> cs msg
  show (CreateJwtException    msg) = "CreateJwtException => " <> cs msg
  show AccessRestricted            = "AccessRestricted"

instance Exception DomainException
