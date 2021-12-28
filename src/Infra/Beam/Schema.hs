module Infra.Beam.Schema
  ( migrateSgmDb
  ) where

import           Database.Beam.Postgres                             ( Connection )
import           Infra.Beam.MigrationUtils                          ( applyMigrations
                                                                    , writeMigration
                                                                    )
import           Infra.Beam.Schema.Latest                           ( migrations )
import           RIO                                                ( (.)
                                                                    , (>>)
                                                                    , MonadIO
                                                                    , liftIO
                                                                    , mapM_
                                                                    , uncurry
                                                                    )


migrateSgmDb :: (MonadIO m) => Connection -> m ()
migrateSgmDb conn = mapM_ (liftIO . uncurry writeMigration) migrations >> applyMigrations conn
