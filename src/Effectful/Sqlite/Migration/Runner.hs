module Effectful.Sqlite.Migration.Runner
  ( runMigrations
  , getMigrationStatus
  , getPendingMigrations
  , createMigrationsTable
  )
where

import Control.Exception (try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List (sortOn)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Database.SQLite.Simple (Connection, Only (..), SQLError, execute_, query_)
import Database.SQLite.Simple qualified as SQL
import Effectful.Sqlite.Migration.Types
import GHC.IO.Exception ()
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- | Run all pending migrations from the given directory
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

-- | Get list of pending (not yet executed) migrations
getPendingMigrations :: Connection -> FilePath -> IO (Either MigrationError [Migration])
getPendingMigrations conn migrationsDir = do
  migrationsResult <- loadMigrationsFromDir migrationsDir
  case migrationsResult of
    Left err -> pure $ Left err
    Right migrations -> do
      executed <- getExecutedVersions conn
      let pending = filter (\m -> m.version `Set.notMember` executed) migrations
      pure $ Right $ sortOn (.version) pending

-- | Get migration status (executed and pending)
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

-- | Create the _migrations table if it doesn't exist
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
      Nothing -> pure $ Left $ InvalidFilename filename
      Just (ver, n) -> do
        contentResult <- try $ BS8.readFile (baseDir </> filename)
        case contentResult of
          Left (err :: IOError) ->
            pure $ Left $ ReadError filename (pack $ show err)
          Right content ->
            pure $ Right $ Migration ver n content

-- | Parse migration filename into (version, name)
parseMigrationFilename :: FilePath -> Maybe (Text, Text)
parseMigrationFilename filename = do
  let baseName = takeBaseName filename
      (versionPart, rest) = break (== '_') baseName
  if length versionPart /= 14 || not (all (`elem` ['0' .. '9']) versionPart)
    then Nothing
    else case rest of
      ('_' : namePart)
        | not (null namePart) ->
            Just (pack versionPart, pack namePart)
      _ -> Nothing

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
    let statements = splitStatements migration.sql
    mapM_ (execute_ conn . SQL.Query . decodeUtf8Lenient) statements
    SQL.execute
      conn
      "INSERT INTO _migrations (version, name) VALUES (?, ?)"
      (migration.version, migration.name)

  splitStatements :: ByteString -> [ByteString]
  splitStatements s =
    filter (not . BS8.all isSpace) $ BS8.split ';' s
   where
    isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

  decodeUtf8Lenient :: ByteString -> Text
  decodeUtf8Lenient = pack . BS8.unpack
