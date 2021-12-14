module Infra.Db.Schema
  ( SgmDatabase(..)
  , getSgmDatabase
  ) where

import           Data.String.Conversions                  ( cs )
import           Database.Beam                            ( DatabaseSettings
                                                          , MonadBeam(..)
                                                          , all_
                                                          , runNoReturn
                                                          , runSelectReturningList
                                                          , select
                                                          )
import           Database.Beam.Migrate                    ( executeMigration
                                                          , runMigrationSilenced
                                                          , runMigrationSteps
                                                          , unCheckDatabase
                                                          )
import           Database.Beam.Migrate.Log                ( BeamMigrateDb(..)
                                                          , LogEntryT(_logEntryCommitId)
                                                          , beamMigrateDb
                                                          , ensureBackendTables
                                                          , recordCommit
                                                          )
import           Database.Beam.Postgres                   ( Pg
                                                          , Postgres(..)
                                                          )
import           Database.Beam.Postgres.Migrate           ( migrationBackend )
import           Infra.Db.Schema.Latest                   ( SgmDatabase(..)
                                                          , migrationSteps
                                                          )
import           Prelude                                  ( String )
import           RIO                                      ( ($)
                                                          , (.)
                                                          , (<&>)
                                                          , (<>)
                                                          , IO
                                                          , Maybe(..)
                                                          , elem
                                                          , liftIO
                                                          , map
                                                          , return
                                                          , show
                                                          )
import           Utils                                    ( uuidFromText )


getSgmDatabase :: (String -> IO ()) -> Pg (DatabaseSettings Postgres SgmDatabase)
getSgmDatabase logFunc = do
  ensureBackendTables migrationBackend
  db <- runMigrationSteps 0 Nothing migrationSteps $ \_ migrationId migration -> do
    let migrationEntriesQuery = select $ all_ (_beamMigrateLogEntries (beamMigrateDb @_ @Pg))
    appliedUUIDs <- runSelectReturningList migrationEntriesQuery <&> map _logEntryCommitId
    if migrationId `elem` appliedUUIDs
      then do
        liftIO . logFunc $ "Migration " <> cs (show migrationId) <> " already applied, skipping"
        return $ runMigrationSilenced migration
      else do
        liftIO . logFunc $ "Applying migration " <> cs (show migrationId)
        appliedMigation <- executeMigration runNoReturn migration
        recordCommit $ uuidFromText migrationId
        return appliedMigation

  return $ unCheckDatabase db
