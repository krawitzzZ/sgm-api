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
  InvalidPassword |
  CreateJwtException Text |
  AccessRestricted Text
  deriving (Eq)

instance Show DomainException where
  show (InternalError         msg) = "InternalError => " <> cs msg
  show (NotFound              msg) = "NotFound => " <> cs msg
  show (UserNameAlreadyExists msg) = "UserNameAlreadyExists => " <> cs msg
  show InvalidPassword             = "InvalidPassword => Provided password is invalid"
  show (CreateJwtException msg)    = "CreateJwtException => " <> cs msg
  show (AccessRestricted   msg)    = "AccessRestricted => " <> cs msg

instance Exception DomainException
