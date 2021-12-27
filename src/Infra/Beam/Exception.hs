module Infra.Beam.Exception
  ( BeamException(..)
  , tryCatchSql
  ) where

import           Control.Exception.Safe                             ( Handler(..)
                                                                    , MonadCatch
                                                                    , MonadThrow
                                                                    , throwM
                                                                    )
import           Data.String.Conversions                            ( cs )
import           Database.Beam.Postgres                             ( SqlError(..) )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<>)
                                                                    , Exception(..)
                                                                    , Show(..)
                                                                    , Text
                                                                    )
import           Utils.Exception                                    ( mkTryCatchDefault )


data BeamException =
  SqlException Text |
  Conflict Text

instance Show BeamException where
  show (SqlException msg) = "SqlException =>> " <> cs msg
  show (Conflict     msg) = "Conflict =>> " <> cs msg

instance Exception BeamException

tryCatchSql :: (MonadCatch m) => m a -> m a
tryCatchSql = mkTryCatchDefault [Handler handleSqlEerror]


handleSqlEerror :: (MonadThrow m) => SqlError -> m a
handleSqlEerror e = throwM $ toBeamException e
 where
  toBeamException sqlErr@(SqlError "23505" _ _ _ _) = Conflict . showSqlErr $ sqlErr
  toBeamException sqlErr                            = SqlException . showSqlErr $ sqlErr
  showSqlErr :: SqlError -> Text
  showSqlErr (SqlError stateCode _execStatus msg detail _hint) =
    "StateCode: " <> cs stateCode <> ", " <> "Error: " <> cs msg <> ", " <> "Detail: " <> cs detail
