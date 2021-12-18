module Infra.Beam.Query
  ( runBeam
  , usersTable
  , eventsTable
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
                                                          , runBeamPostgres
                                                          )
import           Infra.Beam.Schema.Latest                 ( EventEntityT
                                                          , SgmDatabase(..)
                                                          , UserEntityT
                                                          , sgmDb
                                                          )
import           RIO                                      ( (.)
                                                          , (>>=)
                                                          , MonadIO
                                                          , MonadReader
                                                          , asks
                                                          , flip
                                                          , liftIO
                                                          )


runBeam :: (Has Connection e, MonadReader e m, MonadIO m) => Pg a -> m a
runBeam query = asks extract >>= liftIO . flip runBeamPostgres query

usersTable :: DatabaseEntity Postgres SgmDatabase (TableEntity UserEntityT)
usersTable = dbUsers sgmDb

eventsTable :: DatabaseEntity Postgres SgmDatabase (TableEntity EventEntityT)
eventsTable = dbEvents sgmDb
