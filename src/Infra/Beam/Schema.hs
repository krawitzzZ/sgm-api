module Infra.Beam.Schema
  ( migrateSgmDb
  ) where

import           Data.String.Conversions                  ( cs )
import           Database.Beam                            ( all_
                                                          , runNoReturn
                                                          , runSelectReturningList
                                                          , select
                                                          )
import           Database.Beam.Migrate                    ( executeMigration
                                                          , runMigrationSilenced
                                                          , runMigrationSteps
                                                          )
import           Database.Beam.Migrate.Log                ( BeamMigrateDb(..)
                                                          , LogEntryT(..)
                                                          , beamMigrateDb
                                                          , ensureBackendTables
                                                          , recordCommit
                                                          )
import           Database.Beam.Postgres                   ( Connection
                                                          , Pg
                                                          , runBeamPostgresDebug
                                                          )
import           Database.Beam.Postgres.Migrate           ( migrationBackend )
import           Infra.Beam.Schema.Latest                 ( migrationSteps )
import           Infra.Beam.Schema.Types                  ( migrationId )
import           Prelude                                  ( String )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<&>)
                                                          , (<>)
                                                          , IO
                                                          , Maybe(..)
                                                          , MonadIO
                                                          , elem
                                                          , liftIO
                                                          , map
                                                          , return
                                                          , show
                                                          , void
                                                          )


migrateSgmDb :: MonadIO m => Connection -> (String -> IO ()) -> m ()
migrateSgmDb conn logFunc = liftIO . runBeamPostgresDebug logFunc conn $ do
  ensureBackendTables migrationBackend
  void $ runMigrationSteps 0 Nothing migrationSteps $ \_ mId migration -> do
    let migrationEntriesQuery = select $ all_ (_beamMigrateLogEntries (beamMigrateDb @_ @Pg))
    appliedUUIDs <- runSelectReturningList migrationEntriesQuery <&> map _logEntryCommitId
    if mId `elem` appliedUUIDs
      then do
        liftIO . logFunc $ "Migration " <> cs (show mId) <> " already applied, skipping"
        return $ runMigrationSilenced migration
      else do
        liftIO . logFunc $ "Applying migration " <> cs (show mId)
        appliedMigation <- executeMigration runNoReturn migration
        recordCommit $ migrationId mId
        return appliedMigation
