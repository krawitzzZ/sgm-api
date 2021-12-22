module Infra.Beam.Query
  ( runBeam
  , usersTable
  , eventsTable
  , pgCrypto
  ) where

import           Control.Monad.Reader.Has                 ( Has
                                                          , extract
                                                          )
import           Database.Beam                            ( DatabaseEntity
                                                          , TableEntity
                                                          )
import           Database.Beam.Postgres                   ( Connection
                                                          , Pg
                                                          , Postgres
                                                          , getPgExtension
                                                          , runBeamPostgres
                                                          )
import           Database.Beam.Postgres.PgCrypto          ( PgCrypto(..) )
import           Infra.Beam.Schema.Latest                 ( EventEntityT
                                                          , SgmDatabase(..)
                                                          , UserEntityT
                                                          , sgmDb
                                                          )
import           RIO                                      ( ($)
                                                          , MonadIO
                                                          , liftIO
                                                          )


runBeam :: (Has Connection c, MonadIO m) => c -> Pg a -> m a
runBeam c query = liftIO $ runBeamPostgres (extract c) query

usersTable :: DatabaseEntity Postgres SgmDatabase (TableEntity UserEntityT)
usersTable = dbUsers sgmDb

eventsTable :: DatabaseEntity Postgres SgmDatabase (TableEntity EventEntityT)
eventsTable = dbEvents sgmDb

pgCrypto :: PgCrypto
pgCrypto = getPgExtension (dbCryptoExtension sgmDb)
