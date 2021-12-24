module Infra.Exception
  ( DbExceptionn(..)
  , tryCatch
  , tryCatches
  , tryCatchDefault
  ) where

import           Control.Exception.Safe                   ( Handler(..)
                                                          , MonadCatch
                                                          , MonadThrow
                                                          , throwM
                                                          )
import           Data.String.Conversions                  ( cs )
import           Database.Beam.Postgres                   ( SqlError(..) )
import           Domain.App.Class                         ( MonadLogger(..) )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<>)
                                                          , Eq
                                                          , Exception(..)
                                                          , Show(..)
                                                          , Text
                                                          )
import           Utils.Exception                          ( mkTryCatch
                                                          , mkTryCatchDefault
                                                          , mkTryCatches
                                                          )


data DbExceptionn =
  DbExceptionn Text |
  Conflict Text
  deriving (Eq, Show)

instance Exception DbExceptionn

tryCatch :: (MonadLogger m, MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
tryCatch = mkTryCatch (Handler handleSqlException)

tryCatches :: (MonadLogger m, MonadCatch m) => m a -> [Handler m a] -> m a
tryCatches = mkTryCatches (Handler handleSqlException)

tryCatchDefault :: (MonadLogger m, MonadCatch m) => m a -> m a
tryCatchDefault = mkTryCatchDefault [Handler handleSqlException]


handleSqlException :: (MonadLogger m, MonadThrow m) => SqlError -> m a
handleSqlException e = do
  let err = toDbExceptionn e
  withError err $ logWarn "Unexpected SQL exception occurred"
  throwM err

toDbExceptionn :: SqlError -> DbExceptionn
toDbExceptionn e@(SqlError "23505" _ _ _ _) = Conflict . showSqlErr $ e
toDbExceptionn e                            = DbExceptionn . showSqlErr $ e

showSqlErr :: SqlError -> Text
showSqlErr (SqlError stateCode _execStatus msg detail _hint) =
  "StateCode: " <> cs stateCode <> ", " <> "Error: " <> cs msg <> ", " <> "Detail: " <> cs detail
