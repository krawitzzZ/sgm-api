module Infra.Beam.MigrationUtils
  ( applyMigrations
  , migrationString
  , sqlFilename
  , writeMigration
  ) where

import           Control.Exception.Safe                             ( throwM )
import           Database.Beam.Migrate                              ( CheckedDatabaseSettings
                                                                    , Migration
                                                                    )
import           Database.Beam.Migrate.Backend                      ( BeamMigrationBackend(..) )
import           Database.Beam.Migrate.Simple                       ( backendMigrationScript )
import           Database.Beam.Postgres                             ( Connection
                                                                    , Postgres
                                                                    )
import           Database.Beam.Postgres.Migrate                     ( migrationBackend )
import           Database.PostgreSQL.Simple                         ( withTransaction )
import           Database.PostgreSQL.Simple.Migration               ( MigrationCommand(..)
                                                                    , MigrationResult(..)
                                                                    , runMigrations
                                                                    )
import           RIO                                                ( ($)
                                                                    , (.)
                                                                    , (<&>)
                                                                    , (<>)
                                                                    , (>>=)
                                                                    , Bool(..)
                                                                    , Exception
                                                                    , IO
                                                                    , Maybe(..)
                                                                    , MonadIO
                                                                    , Show
                                                                    , String
                                                                    , liftIO
                                                                    , lines
                                                                    , unlines
                                                                    )
import           RIO.Directory                                      ( createDirectoryIfMissing
                                                                    , findFile
                                                                    , getCurrentDirectory
                                                                    )
import           RIO.FilePath                                       ( (-<.>)
                                                                    , (</>)
                                                                    , FilePath
                                                                    )
import           RIO.List.Partial                                   ( tail )
import           System.IO                                          ( putStrLn
                                                                    , writeFile
                                                                    )


applyMigrations :: (MonadIO m) => Connection -> m ()
applyMigrations conn = do
  migrationsDirectoyPath <- liftIO migrationsDirectory
  let migrationCommands = [MigrationInitialization, MigrationDirectory migrationsDirectoyPath]

  liftIO $ withTransaction conn $ runMigrations True conn migrationCommands >>= \case
    MigrationSuccess         -> putStrLn "Migration successful"
    MigrationError migration -> throwM . MigrationException $ "Migration failed: " <> migration

sqlFilename :: FilePath -> FilePath
sqlFilename migrationName = migrationName -<.> ".sql"

migrationString :: Migration Postgres (CheckedDatabaseSettings Postgres db) -> String
migrationString migration =
  let BeamMigrationBackend { backendRenderSyntax } = migrationBackend
      renderedMigration = backendMigrationScript backendRenderSyntax migration
      sanitizedMigration = unlines . tail . lines $ renderedMigration
  in  setEncodingDirective <> sanitizedMigration

writeMigration :: FilePath -> String -> IO ()
writeMigration filename migration = do
  migrationDirectoryPath <- migrationsDirectory
  createDirectoryIfMissing True migrationDirectoryPath

  findFile [migrationDirectoryPath] filename >>= \case
    (Just _) -> putStrLn $ "Migration already exists: " <> filename
    Nothing  -> writeFile (migrationDirectoryPath </> filename) migration

migrationsDirectory :: IO FilePath
migrationsDirectory = getCurrentDirectory <&> (</> "src" </> "Infra" </> "Beam" </> "Migrations")

setEncodingDirective :: String
setEncodingDirective = "SET client_encoding = 'UTF8';\n"


newtype MigrationException = MigrationException String deriving Show
instance Exception MigrationException
