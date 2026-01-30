-- |
-- Module      : Effectful.Sqlite.Effect
-- Description : SQLite database effects and handlers
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
-- Maintainer  : birdeggegg@gmail.com
--
-- This module provides a two-layer effect design for SQLite database operations
-- using the effectful framework. The design enforces transaction boundaries at
-- compile time.
--
-- == Architecture
--
-- The library uses two effects:
--
-- * 'SQLite' - Outer layer for connection pool management
-- * 'SQLiteTransaction' - Inner layer for database operations within a connection context
--
-- == Basic Usage
--
-- @
-- import Data.Pool qualified as Pool
-- import Database.SQLite.Simple qualified as SQL
-- import Effectful
-- import Effectful.Sqlite
--
-- main :: IO ()
-- main = do
--   pool <- Pool.newPool $ Pool.defaultPoolConfig
--     (SQL.open "app.db")
--     SQL.close
--     300
--     10
--
--   runEff . runSQLite pool $ do
--     -- Read without transaction
--     users <- notransact $ query_ "SELECT id, name FROM users"
--     liftIO $ print users
--
--     -- Write with transaction
--     transact $ do
--       execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- @
--
-- == Transaction Boundaries
--
-- All database operations require the 'SQLiteTransaction' effect, which can only
-- be introduced via 'transact', 'transactImmediate', 'transactExclusive', or 'notransact'.
-- This ensures that transaction boundaries are explicit and enforced at compile time.
module Effectful.Sqlite.Effect
  ( -- * Effects
    SQLite
  , SQLiteTransaction

    -- * Handlers
  , runSQLite
  , runSQLiteWithRetry
  , runSQLiteWithConnection
  , runSQLiteWithPath

    -- * Transaction Boundaries
  , transact
  , transactImmediate
  , transactExclusive
  , notransact

    -- * Query Operations
  , query
  , query_
  , execute
  , execute_
  , executeMany
  , queryNamed
  , executeNamed
  , executeNamedReturningChanges

    -- * Execute with Result
  , ExecuteResult (..)
  , executeReturning
  , executeReturning_

    -- * Streaming Operations
  , fold
  , fold_
  , foldNamed
  , forEach
  , forEach_
  , forEachNamed

    -- * Nested Transactions
  , savepoint

    -- * Migration
  , runMigrations
  , getMigrationStatus
  , getPendingMigrations

    -- * Low-level
  , getRawConnection
  )
where

import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Database.SQLite.Simple (Connection, FromRow, NamedParam, Query, ToRow)
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Dispatch.Static
import Effectful.Exception qualified as E
import Effectful.Sqlite.Internal (withResourceEff)
import Effectful.Sqlite.Migration.Runner qualified as M
import Effectful.Sqlite.Migration.Types
import Effectful.Sqlite.Retry (RetryConfig, retryOnBusy)
import Effectful.Sqlite.Types (ExecuteResult (..))

--------------------------------------------------------------------------------
-- Effect Definitions
--------------------------------------------------------------------------------

-- | SQLite effect for connection pool management.
--
-- This is the outer layer effect that manages the connection pool.
-- It provides transaction boundary functions ('transact', 'notransact', etc.)
-- and migration operations.
--
-- Use one of the handlers ('runSQLite', 'runSQLiteWithConnection', or
-- 'runSQLiteWithPath') to interpret this effect.
data SQLite :: Effect

type instance DispatchOf SQLite = 'Static 'WithSideEffects

data instance StaticRep SQLite = SQLiteRep
  { _sqlitePool :: Pool Connection
  , _sqliteRetryConfig :: Maybe RetryConfig
  }

-- | SQLite transaction effect for database operations.
--
-- This is the inner layer effect that provides access to a database connection.
-- All query and execute operations require this effect.
--
-- This effect can only be introduced via:
--
-- * 'transact' - DEFERRED transaction
-- * 'transactImmediate' - IMMEDIATE transaction
-- * 'transactExclusive' - EXCLUSIVE transaction
-- * 'notransact' - Direct connection access without transaction
--
-- This design ensures that transaction boundaries are explicit and
-- enforced at compile time.
data SQLiteTransaction :: Effect

type instance DispatchOf SQLiteTransaction = 'Static 'WithSideEffects

data instance StaticRep SQLiteTransaction = SQLiteTxRep
  { _txConnection :: Connection
  , _txRetryConfig :: Maybe RetryConfig
  }

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Run SQLite effect using a connection pool.
--
-- This is the recommended handler for production use. It manages a pool of
-- database connections and automatically handles connection acquisition and
-- release.
--
-- @
-- import Data.Pool qualified as Pool
-- import Database.SQLite.Simple qualified as SQL
--
-- pool <- Pool.newPool $ Pool.defaultPoolConfig
--   (SQL.open "app.db")  -- create action
--   SQL.close            -- destroy action
--   300                  -- idle timeout (seconds)
--   10                   -- max connections
--
-- runEff . runSQLite pool $ do
--   users <- notransact $ query_ "SELECT * FROM users"
--   ...
-- @
runSQLite ::
  (HasCallStack, IOE :> es) =>
  Pool Connection ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLite pool =
  evalStaticRep (SQLiteRep pool Nothing)

-- | Run SQLite effect using a connection pool with retry on busy.
--
-- This handler extends 'runSQLite' with automatic retry logic for SQLite
-- @ErrorBusy@ errors. When the database is locked by another connection,
-- operations will be retried according to the provided 'RetryConfig'.
--
-- This is recommended for production environments with concurrent database
-- access, as it handles transient locking issues gracefully.
--
-- @
-- import Data.Pool qualified as Pool
-- import Database.SQLite.Simple qualified as SQL
-- import Effectful.Sqlite
--
-- pool <- Pool.newPool $ Pool.defaultPoolConfig
--   (SQL.open "app.db")
--   SQL.close
--   300
--   10
--
-- -- Use default retry configuration
-- runEff . runSQLiteWithRetry defaultRetryConfig pool $ do
--   users <- notransact $ query_ "SELECT * FROM users"
--   ...
--
-- -- Or customize retry behavior
-- let config = RetryConfig
--       { maxRetries = 5
--       , initialDelay = 100_000
--       , backoffFactor = 2.0
--       , maxDelay = 2_000_000
--       }
-- runEff . runSQLiteWithRetry config pool $ do
--   transact $ execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- @
runSQLiteWithRetry ::
  (HasCallStack, IOE :> es) =>
  RetryConfig ->
  Pool Connection ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLiteWithRetry retryConfig pool =
  evalStaticRep (SQLiteRep pool (Just retryConfig))

-- | Run SQLite effect using a single connection (no pool).
--
-- Useful for testing or short-lived operations where connection pooling
-- is not needed. The caller is responsible for opening and closing the
-- connection.
--
-- @
-- conn <- SQL.open ":memory:"
-- result <- runEff . runSQLiteWithConnection conn $ do
--   notransact $ execute_ "CREATE TABLE test (id INTEGER PRIMARY KEY)"
--   notransact $ query_ "SELECT * FROM test"
-- SQL.close conn
-- @
runSQLiteWithConnection ::
  (HasCallStack, IOE :> es) =>
  Connection ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLiteWithConnection conn action = do
  -- Create a single-connection "pool"
  pool <- unsafeEff_ $ Pool.newPool $ Pool.defaultPoolConfig
    (pure conn)      -- return the same connection
    (\_ -> pure ())  -- don't close it
    300
    1
  evalStaticRep (SQLiteRep pool Nothing) action

-- | Run SQLite effect using a database path, automatically managing the connection.
--
-- Opens a connection at the start and closes it when done.
-- The connection is properly closed even if an exception occurs.
-- Useful for one-off scripts or simple applications.
--
-- @
-- runEff . runSQLiteWithPath "app.db" $ do
--   notransact $ execute_ "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)"
--   transact $ execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- @
runSQLiteWithPath ::
  (HasCallStack, IOE :> es) =>
  FilePath ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLiteWithPath dbPath action =
  E.bracket
    (unsafeEff_ $ SQL.open dbPath)
    (unsafeEff_ . SQL.close)
    (\conn -> runSQLiteWithConnection conn action)

--------------------------------------------------------------------------------
-- Transaction Boundaries
--------------------------------------------------------------------------------

-- | Run an action within a DEFERRED transaction.
--
-- DEFERRED is SQLite's default transaction mode. The transaction begins
-- but doesn't acquire any locks until the first read or write operation.
--
-- @
-- transact $ do
--   execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
--   execute "INSERT INTO logs (msg) VALUES (?)" (Only "User created")
-- @
transact ::
  (SQLite :> es, IOE :> es) =>
  Eff (SQLiteTransaction : es) a ->
  Eff es a
transact = transactWith SQL.withTransaction

-- | Run an action within an IMMEDIATE transaction.
--
-- IMMEDIATE mode acquires a write lock immediately when the transaction
-- begins. This prevents other writers from starting transactions.
--
-- Use this when you know you will write and want to fail fast if the
-- database is locked.
transactImmediate ::
  (SQLite :> es, IOE :> es) =>
  Eff (SQLiteTransaction : es) a ->
  Eff es a
transactImmediate = transactWith SQL.withImmediateTransaction

-- | Run an action within an EXCLUSIVE transaction.
--
-- EXCLUSIVE mode acquires an exclusive lock immediately, preventing
-- both readers and writers. This is the most restrictive mode.
--
-- Use sparingly, only when you need to prevent all other access.
transactExclusive ::
  (SQLite :> es, IOE :> es) =>
  Eff (SQLiteTransaction : es) a ->
  Eff es a
transactExclusive = transactWith SQL.withExclusiveTransaction

-- | Internal helper for transaction implementations.
transactWith ::
  (SQLite :> es, IOE :> es) =>
  (Connection -> IO a -> IO a) ->
  Eff (SQLiteTransaction : es) a ->
  Eff es a
transactWith withTx action = do
  SQLiteRep pool mRetryConfig <- getStaticRep
  withResourceEff pool $ \conn ->
    unsafeEff $ \es ->
      withTx conn $
        unEff (evalStaticRep (SQLiteTxRep conn mRetryConfig) action) es

-- | Run an action without a transaction.
--
-- This acquires a connection from the pool and runs the action directly,
-- without starting a transaction. Each operation is auto-committed.
--
-- Use this for read operations or when you need auto-commit behavior.
--
-- @
-- users <- notransact $ query_ "SELECT * FROM users"
-- @
notransact ::
  (SQLite :> es, IOE :> es) =>
  Eff (SQLiteTransaction : es) a ->
  Eff es a
notransact action = do
  SQLiteRep pool mRetryConfig <- getStaticRep
  withResourceEff pool $ \conn ->
    evalStaticRep (SQLiteTxRep conn mRetryConfig) action

--------------------------------------------------------------------------------
-- Query Operations
--------------------------------------------------------------------------------

-- | Execute a query with parameters and return all matching rows.
--
-- @
-- users <- query "SELECT * FROM users WHERE age > ?" (Only 18)
-- @
query ::
  (SQLiteTransaction :> es, IOE :> es, ToRow q, FromRow r) =>
  Query ->
  q ->
  Eff es [r]
query q params = withRetry $ \conn -> SQL.query conn q params

-- | Execute a query without parameters and return all matching rows.
--
-- @
-- users <- query_ "SELECT * FROM users"
-- @
query_ ::
  (SQLiteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  Eff es [r]
query_ q = withRetry $ \conn -> SQL.query_ conn q

-- | Execute a statement with parameters (INSERT, UPDATE, DELETE).
--
-- @
-- execute "INSERT INTO users (name, age) VALUES (?, ?)" ("Alice", 30)
-- @
execute ::
  (SQLiteTransaction :> es, IOE :> es, ToRow q) =>
  Query ->
  q ->
  Eff es ()
execute q params = withRetry $ \conn -> SQL.execute conn q params

-- | Execute a statement without parameters.
--
-- @
-- execute_ "DELETE FROM temp_data"
-- @
execute_ ::
  (SQLiteTransaction :> es, IOE :> es) =>
  Query ->
  Eff es ()
execute_ q = withRetry $ \conn -> SQL.execute_ conn q

-- | Execute a statement with multiple parameter sets.
--
-- @
-- executeMany "INSERT INTO users (name) VALUES (?)"
--   [ Only "Alice"
--   , Only "Bob"
--   , Only "Charlie"
--   ]
-- @
executeMany ::
  (SQLiteTransaction :> es, IOE :> es, ToRow q) =>
  Query ->
  [q] ->
  Eff es ()
executeMany q params = withRetry $ \conn -> SQL.executeMany conn q params

-- | Execute a query with named parameters.
--
-- @
-- users <- queryNamed
--   "SELECT * FROM users WHERE name = :name AND age > :age"
--   [":name" := ("Alice" :: Text), ":age" := (18 :: Int)]
-- @
queryNamed ::
  (SQLiteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  [NamedParam] ->
  Eff es [r]
queryNamed q params = withRetry $ \conn -> SQL.queryNamed conn q params

-- | Execute a statement with named parameters.
--
-- @
-- executeNamed
--   "INSERT INTO users (name, age) VALUES (:name, :age)"
--   [":name" := ("Alice" :: Text), ":age" := (30 :: Int)]
-- @
executeNamed ::
  (SQLiteTransaction :> es, IOE :> es) =>
  Query ->
  [NamedParam] ->
  Eff es ()
executeNamed q params = withRetry $ \conn -> SQL.executeNamed conn q params

-- | Execute a statement with named parameters and return the number of changed rows.
--
-- @
-- changed <- executeNamedReturningChanges
--   "UPDATE users SET active = :active WHERE role = :role"
--   [":active" := True, ":role" := ("admin" :: Text)]
-- @
executeNamedReturningChanges ::
  (SQLiteTransaction :> es, IOE :> es) =>
  Query ->
  [NamedParam] ->
  Eff es Int
executeNamedReturningChanges q params = withRetry $ \conn -> do
  SQL.executeNamed conn q params
  SQL.changes conn

-- | Execute a statement and return execution metadata.
--
-- @
-- result <- executeReturning "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- liftIO $ putStrLn $ "Inserted user with ID: " <> show result.lastRowId
-- @
executeReturning ::
  (SQLiteTransaction :> es, IOE :> es, ToRow q) =>
  Query ->
  q ->
  Eff es ExecuteResult
executeReturning q params = withRetry $ \conn -> do
  SQL.execute conn q params
  changed <- SQL.changes conn
  rowId <- SQL.lastInsertRowId conn
  pure ExecuteResult {changedRows = changed, lastRowId = rowId}

-- | Execute a statement without parameters and return execution metadata.
--
-- @
-- result <- executeReturning_ "INSERT INTO counters DEFAULT VALUES"
-- @
executeReturning_ ::
  (SQLiteTransaction :> es, IOE :> es) =>
  Query ->
  Eff es ExecuteResult
executeReturning_ q = withRetry $ \conn -> do
  SQL.execute_ conn q
  changed <- SQL.changes conn
  rowId <- SQL.lastInsertRowId conn
  pure ExecuteResult {changedRows = changed, lastRowId = rowId}

--------------------------------------------------------------------------------
-- Streaming Operations
--------------------------------------------------------------------------------

-- | Fold over query results with parameters, processing rows one at a time.
--
-- This is useful for processing large result sets without loading all rows
-- into memory at once.
--
-- @
-- total <- fold "SELECT amount FROM transactions WHERE user_id = ?" (Only userId) 0 $ \\acc (Only amount) ->
--   pure (acc + amount)
-- @
fold ::
  (SQLiteTransaction :> es, IOE :> es, ToRow q, FromRow r) =>
  Query ->
  q ->
  a ->
  (a -> r -> IO a) ->
  Eff es a
fold q params acc f = do
  SQLiteTxRep conn mRetryConfig <- getStaticRep
  unsafeEff_ $ case mRetryConfig of
    Nothing -> SQL.fold conn q params acc f
    Just cfg -> retryOnBusy cfg $ SQL.fold conn q params acc f

-- | Fold over query results without parameters.
--
-- @
-- count <- fold_ "SELECT * FROM users" 0 $ \\acc _ -> pure (acc + 1)
-- @
fold_ ::
  (SQLiteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  a ->
  (a -> r -> IO a) ->
  Eff es a
fold_ q acc f = do
  SQLiteTxRep conn mRetryConfig <- getStaticRep
  unsafeEff_ $ case mRetryConfig of
    Nothing -> SQL.fold_ conn q acc f
    Just cfg -> retryOnBusy cfg $ SQL.fold_ conn q acc f

-- | Fold over query results with named parameters.
--
-- @
-- total <- foldNamed
--   "SELECT amount FROM transactions WHERE status = :status"
--   [":status" := ("completed" :: Text)]
--   0
--   (\\acc (Only amount) -> pure (acc + amount))
-- @
foldNamed ::
  (SQLiteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  [NamedParam] ->
  a ->
  (a -> r -> IO a) ->
  Eff es a
foldNamed q params acc f = do
  SQLiteTxRep conn mRetryConfig <- getStaticRep
  unsafeEff_ $ case mRetryConfig of
    Nothing -> SQL.foldNamed conn q params acc f
    Just cfg -> retryOnBusy cfg $ SQL.foldNamed conn q params acc f

-- | Execute an action for each row in the result set.
--
-- This is a convenience wrapper around 'fold' that discards the accumulator.
--
-- @
-- forEach "SELECT id, name FROM users WHERE active = ?" (Only True) $ \\(userId, name) ->
--   liftIO $ print (userId, name)
-- @
forEach ::
  (SQLiteTransaction :> es, IOE :> es, ToRow q, FromRow r) =>
  Query ->
  q ->
  (r -> IO ()) ->
  Eff es ()
forEach q params f = fold q params () (\_ r -> f r)

-- | Execute an action for each row in the result set (no parameters).
--
-- @
-- forEach_ "SELECT * FROM users" $ \\user ->
--   liftIO $ print user
-- @
forEach_ ::
  (SQLiteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  (r -> IO ()) ->
  Eff es ()
forEach_ q f = fold_ q () (\_ r -> f r)

-- | Execute an action for each row in the result set (named parameters).
--
-- @
-- forEachNamed "SELECT * FROM users WHERE role = :role" [":role" := ("admin" :: Text)] $ \\user ->
--   liftIO $ print user
-- @
forEachNamed ::
  (SQLiteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  [NamedParam] ->
  (r -> IO ()) ->
  Eff es ()
forEachNamed q params f = foldNamed q params () (\_ r -> f r)

--------------------------------------------------------------------------------
-- Nested Transactions (Savepoints)
--------------------------------------------------------------------------------

-- | Run an action within a savepoint (nested transaction).
--
-- Savepoints allow nested transaction-like behavior within an existing
-- transaction. If the inner action fails, only changes since the savepoint
-- are rolled back.
--
-- @
-- transact $ do
--   execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
--   result <- savepoint $ do
--     execute "INSERT INTO users (name) VALUES (?)" (Only "Bob")
--     -- If this fails, only Bob's insert is rolled back
--     execute "INSERT INTO users (name) VALUES (?)" (Only "Charlie")
--   -- Alice's insert is preserved even if savepoint fails
-- @
savepoint ::
  (SQLiteTransaction :> es, IOE :> es) =>
  Eff es a ->
  Eff es a
savepoint action = do
  SQLiteTxRep conn _ <- getStaticRep
  unsafeEff $ \es -> SQL.withSavepoint conn $ unEff action es

--------------------------------------------------------------------------------
-- Migration Operations
--------------------------------------------------------------------------------

-- | Run all pending migrations from the given directory.
--
-- This function:
--
-- 1. Creates the @_migrations@ table if it doesn't exist
-- 2. Loads all @.sql@ files from the directory
-- 3. Filters out already-executed migrations
-- 4. Executes pending migrations in version order within a transaction
--
-- Migration files should be named: @{14-digit-version}_{name}.sql@
--
-- @
-- result <- runMigrations "migrations/"
-- case result of
--   Left err -> liftIO $ putStrLn $ "Migration failed: " <> show err
--   Right MigrationNoOp -> liftIO $ putStrLn "No migrations to run"
--   Right (MigrationSuccess n) -> liftIO $ putStrLn $ "Ran " <> show n <> " migrations"
-- @
runMigrations ::
  (SQLite :> es, IOE :> es) =>
  FilePath ->
  Eff es (Either MigrationError MigrationResult)
runMigrations dir = do
  SQLiteRep pool _ <- getStaticRep
  withResourceEff pool $ \conn ->
    unsafeEff_ $ M.runMigrations conn dir

-- | Get status of all migrations (executed and pending).
--
-- Returns a tuple of:
--
-- * List of 'MigrationRecord' for already-executed migrations
-- * List of 'Migration' for pending migrations
--
-- Useful for displaying migration status in admin interfaces or CLI tools.
getMigrationStatus ::
  (SQLite :> es, IOE :> es) =>
  FilePath ->
  Eff es (Either MigrationError ([MigrationRecord], [Migration]))
getMigrationStatus dir = do
  SQLiteRep pool _ <- getStaticRep
  withResourceEff pool $ \conn ->
    unsafeEff_ $ M.getMigrationStatus conn dir

-- | Get list of pending (not yet executed) migrations.
--
-- Pending migrations are those that exist in the migrations directory
-- but have not been recorded in the @_migrations@ table.
-- Results are sorted by version in ascending order.
getPendingMigrations ::
  (SQLite :> es, IOE :> es) =>
  FilePath ->
  Eff es (Either MigrationError [Migration])
getPendingMigrations dir = do
  SQLiteRep pool _ <- getStaticRep
  withResourceEff pool $ \conn ->
    unsafeEff_ $ M.getPendingMigrations conn dir

--------------------------------------------------------------------------------
-- Low-level Operations
--------------------------------------------------------------------------------

-- | Get the raw SQLite connection.
--
-- This is useful for advanced operations not covered by the effect API,
-- such as using prepared statements or accessing SQLite-specific features.
--
-- __Warning:__ Be careful not to start transactions or modify connection
-- state in ways that conflict with the effect's transaction management.
--
-- @
-- transact $ do
--   conn <- getRawConnection
--   liftIO $ SQL.lastInsertRowId conn
-- @
getRawConnection ::
  (SQLiteTransaction :> es) =>
  Eff es Connection
getRawConnection = do
  SQLiteTxRep conn _ <- getStaticRep
  pure conn

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Internal helper for operations with optional retry.
withRetry ::
  (SQLiteTransaction :> es, IOE :> es) =>
  (Connection -> IO a) ->
  Eff es a
withRetry f = do
  SQLiteTxRep conn mRetryConfig <- getStaticRep
  unsafeEff_ $ case mRetryConfig of
    Nothing -> f conn
    Just cfg -> retryOnBusy cfg (f conn)
