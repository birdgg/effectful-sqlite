-- |
-- Module      : Effectful.Sqlite.Migration.Runner
-- Description : Migration execution engine
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
-- Maintainer  : birdeggegg@gmail.com
--
-- This module provides the migration execution logic for applying
-- database schema changes.
--
-- == Usage
--
-- Migrations are typically run at application startup:
--
-- @
-- runEff . runSQLite pool $ do
--   result <- runMigrations "migrations/"
--   case result of
--     Left err -> liftIO $ putStrLn $ "Migration failed: " <> show err
--     Right MigrationNoOp -> liftIO $ putStrLn "No migrations to run"
--     Right (MigrationSuccess n) -> liftIO $ putStrLn $ "Ran " <> show n <> " migrations"
-- @
--
-- == Migration Files
--
-- Place SQL files in your migrations directory with the naming format:
--
-- @
-- {14-digit-version}_{name}.sql
-- @
--
-- For example:
--
-- @
-- migrations/
--   20240114120000_create_users.sql
--   20240114130000_add_email_to_users.sql
-- @
module Effectful.Sqlite.Migration.Runner
  ( runMigrations
  , getMigrationStatus
  , getPendingMigrations
  , createMigrationsTable
  )
where

import Control.Exception (try)
import Data.ByteString.Char8 qualified as BS8
import Data.List (sortOn)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple (Connection, Only (..), SQLError, execute_, query_)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as SQLite3
import Effectful.Sqlite.Migration.Types
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- | Run all pending migrations from the given directory.
--
-- This function:
--
-- 1. Creates the @_migrations@ table if it doesn't exist
-- 2. Loads all @.sql@ files from the directory
-- 3. Filters out already-executed migrations
-- 4. Executes pending migrations in version order within a transaction
--
-- Returns 'MigrationNoOp' if there are no pending migrations,
-- or 'MigrationSuccess' with the count of executed migrations.
runMigrations :: Connection -> FilePath -> IO (Either MigrationError MigrationResult)
runMigrations conn migrationsDir = do
  exists <- doesDirectoryExist migrationsDir
  if not exists
    then pure $ Left $ DirectoryNotFound migrationsDir
    else do
      createMigrationsTable conn
      pendingResult <- getPendingMigrations conn migrationsDir
      case pendingResult of
        Left err -> pure $ Left err
        Right [] -> pure $ Right MigrationNoOp
        Right pending -> runPendingMigrations conn pending

-- | Get list of pending (not yet executed) migrations.
--
-- Pending migrations are those that exist in the migrations directory
-- but have not been recorded in the @_migrations@ table.
-- Results are sorted by version in ascending order.
getPendingMigrations :: Connection -> FilePath -> IO (Either MigrationError [Migration])
getPendingMigrations conn migrationsDir = do
  migrationsResult <- loadMigrationsFromDir migrationsDir
  case migrationsResult of
    Left err -> pure $ Left err
    Right migrations -> do
      executed <- getExecutedVersions conn
      let pending = filter (\m -> m.version `Set.notMember` executed) migrations
      pure $ Right $ sortOn (.version) pending

-- | Get migration status showing both executed and pending migrations.
--
-- Returns a tuple of:
--
-- * List of 'MigrationRecord' for already-executed migrations
-- * List of 'Migration' for pending migrations
--
-- Useful for displaying migration status in admin interfaces or CLI tools.
getMigrationStatus :: Connection -> FilePath -> IO (Either MigrationError ([MigrationRecord], [Migration]))
getMigrationStatus conn migrationsDir = do
  exists <- doesDirectoryExist migrationsDir
  if not exists
    then pure $ Left $ DirectoryNotFound migrationsDir
    else do
      createMigrationsTable conn
      executed <- getExecutedMigrations conn
      pendingResult <- getPendingMigrations conn migrationsDir
      case pendingResult of
        Left err -> pure $ Left err
        Right pending -> pure $ Right (executed, pending)

-- | Create the @_migrations@ table if it doesn't exist.
--
-- The table schema is:
--
-- @
-- CREATE TABLE _migrations (
--   version TEXT PRIMARY KEY NOT NULL,
--   name TEXT NOT NULL,
--   executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
-- )
-- @
--
-- This is called automatically by 'runMigrations' and 'getMigrationStatus',
-- but can be called explicitly if needed.
createMigrationsTable :: Connection -> IO ()
createMigrationsTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS _migrations (\
    \  version TEXT PRIMARY KEY NOT NULL,\
    \  name TEXT NOT NULL,\
    \  executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL\
    \)"

-- | Load all migration files from directory
loadMigrationsFromDir :: FilePath -> IO (Either MigrationError [Migration])
loadMigrationsFromDir dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure $ Left $ DirectoryNotFound dir
    else do
      filesResult <- try $ listDirectory dir
      case filesResult of
        Left (err :: IOError) ->
          pure $ Left $ ReadError dir (pack $ show err)
        Right files -> do
          let sqlFiles = filter (\f -> takeExtension f == ".sql") files
          loadMigrationFiles dir sqlFiles
 where
  loadMigrationFiles :: FilePath -> [FilePath] -> IO (Either MigrationError [Migration])
  loadMigrationFiles baseDir sqlFiles = do
    results <- mapM (loadSingleMigration baseDir) sqlFiles
    pure $ sequence results

  loadSingleMigration :: FilePath -> FilePath -> IO (Either MigrationError Migration)
  loadSingleMigration baseDir filename =
    case parseMigrationFilename filename of
      Left reason -> pure $ Left $ InvalidFilename filename reason
      Right (ver, n) -> do
        contentResult <- try $ BS8.readFile (baseDir </> filename)
        case contentResult of
          Left (err :: IOError) ->
            pure $ Left $ ReadError filename (pack $ show err)
          Right content ->
            pure $ Right $ Migration ver n content

-- | Parse migration filename into (version, name) or return an error reason
parseMigrationFilename :: FilePath -> Either Text (Text, Text)
parseMigrationFilename filename =
  let baseName = takeBaseName filename
      (versionPart, rest) = break (== '_') baseName
  in case () of
    _ | length versionPart /= 14 ->
          Left $ "version must be exactly 14 digits, got " <> pack (show (length versionPart))
      | not (all (`elem` ['0' .. '9']) versionPart) ->
          Left "version must contain only digits (0-9)"
      | null rest || rest == "_" ->
          Left "missing name part after version (expected {version}_{name}.sql)"
      | otherwise ->
          case rest of
            ('_' : namePart)
              | not (null namePart) -> Right (pack versionPart, pack namePart)
              | otherwise -> Left "name cannot be empty"
            _ -> Left "version and name must be separated by underscore"

-- | Get set of executed migration versions
getExecutedVersions :: Connection -> IO (Set.Set Text)
getExecutedVersions conn = do
  rows <- query_ conn "SELECT version FROM _migrations" :: IO [Only Text]
  pure $ Set.fromList $ map fromOnly rows

-- | Get all executed migration records
getExecutedMigrations :: Connection -> IO [MigrationRecord]
getExecutedMigrations conn =
  query_ conn "SELECT version, name, executed_at FROM _migrations ORDER BY version"

-- | Run pending migrations in a transaction
runPendingMigrations :: Connection -> [Migration] -> IO (Either MigrationError MigrationResult)
runPendingMigrations conn migrations = do
  result <- try $ SQL.withTransaction conn $ do
    mapM_ executeMigration migrations
  case result of
    Left (err :: SQLError) ->
      pure $ Left $ SqlError "Migration failed" (pack $ show err)
    Right _ ->
      pure $ Right $ MigrationSuccess (length migrations)
 where
  executeMigration :: Migration -> IO ()
  executeMigration migration = do
    -- Use direct-sqlite's exec which properly handles multiple statements
    -- separated by semicolons, including those within string literals
    let sqlText = TE.decodeUtf8Lenient migration.sql
    SQLite3.exec (SQL.connectionHandle conn) sqlText
    SQL.execute
      conn
      "INSERT INTO _migrations (version, name) VALUES (?, ?)"
      (migration.version, migration.name)
