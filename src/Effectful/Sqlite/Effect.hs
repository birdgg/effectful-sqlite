{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Effectful.Sqlite.Effect
-- Description : SQLite database effect and handlers
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
-- Maintainer  : birdeggegg@gmail.com
--
-- This module provides the core 'SQLite' effect and its handlers for
-- performing database operations using the effectful framework.
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
--     users <- query_ "SELECT id, name FROM users"
--     liftIO $ print users
-- @
--
-- == Transaction Support
--
-- Use 'withTransaction' for automatic transaction management:
--
-- @
-- withTransaction $ do
--   execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
--   execute "INSERT INTO logs (msg) VALUES (?)" (Only "User created")
-- @
module Effectful.Sqlite.Effect
  ( -- * Effect
    SQLite

    -- * Handlers
  , runSQLite
  , runSQLiteWithRetry
  , runSQLiteWithConnection
  , runSQLiteWithPath

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

    -- * Transaction
    -- $transactions
  , withTransaction
  , withSavepoint
  , withImmediateTransaction
  , withExclusiveTransaction
  , begin
  , commit
  , rollback

    -- * Migration
  , runMigrations
  , getMigrationStatus
  , getPendingMigrations
  , createMigrationsTable

    -- * Low-level
  , withConnection
  )
where

import Data.IORef (newIORef)
import Data.Pool (Pool)
import Database.SQLite.Simple (Connection, FromRow, NamedParam, Query, ToRow)
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception qualified as E
import Effectful.Sqlite.Internal (ConnStrategy, poolStrategy, poolStrategyWithRetry, singleConnStrategy, strategyBegin, strategyEndTx, strategyUnliftWithConn, strategyWithConn)
import Effectful.Sqlite.Retry (RetryConfig)
import Effectful.Sqlite.Migration.Runner qualified as M
import Effectful.Sqlite.Migration.Types
import Effectful.Sqlite.Types (ExecuteResult (..))
import Effectful.TH

-- $transactions
--
-- SQLite supports several transaction modes:
--
-- * 'withTransaction' - Default DEFERRED transaction
-- * 'withImmediateTransaction' - Acquires write lock immediately
-- * 'withExclusiveTransaction' - Acquires exclusive lock
-- * 'withSavepoint' - Nested transaction using savepoints
--
-- For manual transaction control, use 'begin', 'commit', and 'rollback'.
-- However, prefer the @with*@ variants as they provide exception safety.

-- | SQLite database effect.
--
-- This effect provides type-safe database operations including:
--
-- * Query execution with automatic parameter binding
-- * Transaction management (automatic and manual)
-- * Streaming result processing via fold operations
-- * Database migrations
--
-- Use one of the handlers ('runSQLite', 'runSQLiteWithConnection', or
-- 'runSQLiteWithPath') to interpret this effect.
data SQLite :: Effect where
  -- | Execute a query with parameters and return all matching rows.
  Query :: (ToRow q, FromRow r) => Query -> q -> SQLite m [r]
  -- | Execute a query without parameters and return all matching rows.
  Query_ :: (FromRow r) => Query -> SQLite m [r]
  -- | Execute a statement with parameters (INSERT, UPDATE, DELETE).
  Execute :: (ToRow q) => Query -> q -> SQLite m ()
  -- | Execute a statement without parameters.
  Execute_ :: Query -> SQLite m ()
  -- | Execute a statement with multiple parameter sets.
  ExecuteMany :: (ToRow q) => Query -> [q] -> SQLite m ()
  -- | Execute a query with named parameters.
  QueryNamed :: (FromRow r) => Query -> [NamedParam] -> SQLite m [r]
  -- | Execute a statement with named parameters.
  ExecuteNamed :: Query -> [NamedParam] -> SQLite m ()
  -- | Execute a statement with named parameters and return the number of changed rows.
  ExecuteNamedReturningChanges :: Query -> [NamedParam] -> SQLite m Int
  -- | Execute a statement and return execution metadata.
  ExecuteReturning :: (ToRow q) => Query -> q -> SQLite m ExecuteResult
  -- | Execute a statement without parameters and return execution metadata.
  ExecuteReturning_ :: Query -> SQLite m ExecuteResult
  -- | Fold over query results with parameters, processing rows one at a time.
  Fold :: (ToRow q, FromRow r) => Query -> q -> a -> (a -> r -> m a) -> SQLite m a
  -- | Fold over query results without parameters.
  Fold_ :: (FromRow r) => Query -> a -> (a -> r -> m a) -> SQLite m a
  -- | Fold over query results with named parameters.
  FoldNamed :: (FromRow r) => Query -> [NamedParam] -> a -> (a -> r -> m a) -> SQLite m a
  -- | Run an action within a DEFERRED transaction.
  WithTransaction :: m a -> SQLite m a
  -- | Run an action within a savepoint (nested transaction).
  WithSavepoint :: m a -> SQLite m a
  -- | Run an action within an IMMEDIATE transaction.
  WithImmediateTransaction :: m a -> SQLite m a
  -- | Run an action within an EXCLUSIVE transaction.
  WithExclusiveTransaction :: m a -> SQLite m a
  -- | Begin a manual transaction. Prefer 'withTransaction' for exception safety.
  Begin :: SQLite m ()
  -- | Commit the current manual transaction.
  Commit :: SQLite m ()
  -- | Rollback the current manual transaction.
  Rollback :: SQLite m ()
  -- | Run all pending migrations from the given directory.
  RunMigrations :: FilePath -> SQLite m (Either MigrationError MigrationResult)
  -- | Get status of all migrations (executed and pending).
  GetMigrationStatus :: FilePath -> SQLite m (Either MigrationError ([MigrationRecord], [Migration]))
  -- | Get list of pending migrations.
  GetPendingMigrations :: FilePath -> SQLite m (Either MigrationError [Migration])
  -- | Create the migrations table if it doesn't exist.
  CreateMigrationsTable :: SQLite m ()
  -- | Run an action with direct access to the underlying connection.
  WithConnection :: (Connection -> m a) -> SQLite m a

type instance DispatchOf SQLite = Dynamic

makeEffect ''SQLite

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
--   users <- query_ "SELECT * FROM users"
--   ...
-- @
runSQLite ::
  (HasCallStack, IOE :> es) =>
  Pool Connection ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLite pool action = do
  txCtx <- liftIO $ newIORef Nothing
  let strategy = poolStrategy pool txCtx
  interpret (handleSQLiteWith strategy) action

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
--   users <- query_ "SELECT * FROM users"
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
--   execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- @
runSQLiteWithRetry ::
  (HasCallStack, IOE :> es) =>
  RetryConfig ->
  Pool Connection ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLiteWithRetry retryConfig pool action = do
  txCtx <- liftIO $ newIORef Nothing
  let strategy = poolStrategyWithRetry retryConfig pool txCtx
  interpret (handleSQLiteWith strategy) action

-- | Run SQLite effect using a single connection (no pool).
--
-- Useful for testing or short-lived operations where connection pooling
-- is not needed. The caller is responsible for opening and closing the
-- connection.
--
-- @
-- conn <- SQL.open ":memory:"
-- result <- runEff . runSQLiteWithConnection conn $ do
--   execute_ "CREATE TABLE test (id INTEGER PRIMARY KEY)"
--   query_ "SELECT * FROM test"
-- SQL.close conn
-- @
runSQLiteWithConnection ::
  (HasCallStack, IOE :> es) =>
  Connection ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLiteWithConnection conn =
  let strategy = singleConnStrategy conn
   in interpret (handleSQLiteWith strategy)

-- | Run SQLite effect using a database path, automatically managing the connection.
--
-- Opens a connection at the start and closes it when done.
-- The connection is properly closed even if an exception occurs.
-- Useful for one-off scripts or simple applications.
--
-- @
-- runEff . runSQLiteWithPath "app.db" $ do
--   execute_ "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)"
--   execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- @
runSQLiteWithPath ::
  (HasCallStack, IOE :> es) =>
  FilePath ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLiteWithPath dbPath action =
  E.bracket
    (liftIO $ SQL.open dbPath)
    (liftIO . SQL.close)
    (\conn -> runSQLiteWithConnection conn action)

-- | Generic SQLite effect handler parameterized by connection strategy.
--
-- This is an internal handler that eliminates code duplication between
-- pool-based and single-connection handlers.
handleSQLiteWith ::
  (IOE :> es) =>
  ConnStrategy es ->
  EffectHandler SQLite es
handleSQLiteWith strategy env = \case
  Query q params ->
    strategyWithConn strategy $ \conn -> SQL.query conn q params
  Query_ q ->
    strategyWithConn strategy $ \conn -> SQL.query_ conn q
  Execute q params ->
    strategyWithConn strategy $ \conn -> SQL.execute conn q params
  Execute_ q ->
    strategyWithConn strategy $ \conn -> SQL.execute_ conn q
  ExecuteMany q params ->
    strategyWithConn strategy $ \conn -> SQL.executeMany conn q params
  QueryNamed q params ->
    strategyWithConn strategy $ \conn -> SQL.queryNamed conn q params
  ExecuteNamed q params ->
    strategyWithConn strategy $ \conn -> SQL.executeNamed conn q params
  ExecuteNamedReturningChanges q params ->
    strategyWithConn strategy $ \conn -> do
      SQL.executeNamed conn q params
      SQL.changes conn
  ExecuteReturning q params ->
    strategyWithConn strategy $ \conn -> do
      SQL.execute conn q params
      changed <- SQL.changes conn
      rowId <- SQL.lastInsertRowId conn
      pure ExecuteResult {changedRows = changed, lastRowId = rowId}
  ExecuteReturning_ q ->
    strategyWithConn strategy $ \conn -> do
      SQL.execute_ conn q
      changed <- SQL.changes conn
      rowId <- SQL.lastInsertRowId conn
      pure ExecuteResult {changedRows = changed, lastRowId = rowId}
  Fold q params acc f ->
    strategyUnliftWithConn strategy env $ \conn unlift ->
      SQL.fold conn q params acc (\a r -> unlift (f a r))
  Fold_ q acc f ->
    strategyUnliftWithConn strategy env $ \conn unlift ->
      SQL.fold_ conn q acc (\a r -> unlift (f a r))
  FoldNamed q params acc f ->
    strategyUnliftWithConn strategy env $ \conn unlift ->
      SQL.foldNamed conn q params acc (\a r -> unlift (f a r))
  WithTransaction innerAction ->
    strategyUnliftWithConn strategy env $ \conn unlift ->
      SQL.withTransaction conn (unlift innerAction)
  WithSavepoint innerAction ->
    strategyUnliftWithConn strategy env $ \conn unlift ->
      SQL.withSavepoint conn (unlift innerAction)
  WithImmediateTransaction innerAction ->
    strategyUnliftWithConn strategy env $ \conn unlift ->
      SQL.withImmediateTransaction conn (unlift innerAction)
  WithExclusiveTransaction innerAction ->
    strategyUnliftWithConn strategy env $ \conn unlift ->
      SQL.withExclusiveTransaction conn (unlift innerAction)
  Begin ->
    strategyBegin strategy
  Commit -> do
    strategyWithConn strategy $ \conn -> SQL.execute_ conn "COMMIT"
    strategyEndTx strategy
  Rollback -> do
    strategyWithConn strategy $ \conn -> SQL.execute_ conn "ROLLBACK"
    strategyEndTx strategy
  RunMigrations dir ->
    strategyWithConn strategy $ \conn -> M.runMigrations conn dir
  GetMigrationStatus dir ->
    strategyWithConn strategy $ \conn -> M.getMigrationStatus conn dir
  GetPendingMigrations dir ->
    strategyWithConn strategy $ \conn -> M.getPendingMigrations conn dir
  CreateMigrationsTable ->
    strategyWithConn strategy M.createMigrationsTable
  WithConnection f ->
    strategyUnliftWithConn strategy env $ \conn unlift ->
      unlift (f conn)

-- | Execute an action for each row in the result set.
--
-- This is a convenience wrapper around 'fold' that discards the accumulator.
--
-- @
-- forEach "SELECT id, name FROM users WHERE active = ?" (Only True) $ \\user ->
--   liftIO $ print user
-- @
forEach ::
  (HasCallStack, SQLite :> es, ToRow q, FromRow r) =>
  Query ->
  q ->
  (r -> Eff es ()) ->
  Eff es ()
forEach q params f = fold q params () (\_ r -> f r)

-- | Execute an action for each row in the result set (no parameters).
--
-- @
-- forEach_ "SELECT * FROM users" $ \\user ->
--   liftIO $ print user
-- @
forEach_ ::
  (HasCallStack, SQLite :> es, FromRow r) =>
  Query ->
  (r -> Eff es ()) ->
  Eff es ()
forEach_ q f = fold_ q () (\_ r -> f r)

-- | Execute an action for each row in the result set (named parameters).
--
-- @
-- forEachNamed "SELECT * FROM users WHERE role = :role" [":role" := ("admin" :: Text)] $ \\user ->
--   liftIO $ print user
-- @
forEachNamed ::
  (HasCallStack, SQLite :> es, FromRow r) =>
  Query ->
  [NamedParam] ->
  (r -> Eff es ()) ->
  Eff es ()
forEachNamed q params f = foldNamed q params () (\_ r -> f r)
