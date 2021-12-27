module Infra.Beam.Query
  ( runBeam
  , pgCrypto
  , usersTable
  , eventsTable
  , userEventAttendancePivot
  ) where

import           Control.Monad.Reader.Has                           ( Has
                                                                    , extract
                                                                    )
import           Database.Beam                                      ( DatabaseEntity
                                                                    , TableEntity
                                                                    )
import           Database.Beam.Postgres                             ( Connection
                                                                    , Pg
                                                                    , Postgres
                                                                    , getPgExtension
                                                                    , runBeamPostgres
                                                                    , runBeamPostgresDebug
                                                                    )
import           Database.Beam.Postgres.PgCrypto                    ( PgCrypto )
import           Domain.App.Config                                  ( Config(..) )
import           Domain.Logger                                      ( LogLevel(..) )
import           Infra.Beam.Exception                               ( tryCatchSql )
import           Infra.Beam.Schema.Latest                           ( EventEntityT
                                                                    , SgmDatabase(..)
                                                                    , UserEntityT
                                                                    , UserEventAttendancePivotT(..)
                                                                    , sgmDb
                                                                    )
import           Prelude                                            ( putStrLn )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (==)
                                                                    , MonadIO
                                                                    , liftIO
                                                                    )


runBeam :: (Has Connection e, Has Config e, MonadIO m) => e -> Pg a -> m a
runBeam e query = do
  let logLevel = cLogLevel . extract $ e
  let runBeam' = if logLevel == Debug then runBeamPostgresDebug putStrLn else runBeamPostgres
  liftIO $ tryCatchSql $ runBeam' (extract e) query

pgCrypto :: PgCrypto
pgCrypto = getPgExtension (dbCryptoExtension sgmDb)

usersTable :: DatabaseEntity Postgres SgmDatabase (TableEntity UserEntityT)
usersTable = dbUsers sgmDb

eventsTable :: DatabaseEntity Postgres SgmDatabase (TableEntity EventEntityT)
eventsTable = dbEvents sgmDb

userEventAttendancePivot
  :: DatabaseEntity Postgres SgmDatabase (TableEntity UserEventAttendancePivotT)
userEventAttendancePivot = dbUserEventAttendance sgmDb
