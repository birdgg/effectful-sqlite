-- |
-- Module      : Effectful.Sqlite
-- Description : Effectful bindings for SQLite with connection pooling and migrations
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
-- Maintainer  : birdeggegg@gmail.com
--
-- This library provides type-safe SQLite database bindings for the
-- @effectful@ ecosystem, with support for connection pooling and migrations.
--
-- == Architecture
--
-- The library uses a two-layer effect design:
--
-- * 'SQLite' - Outer layer for connection pool management
-- * 'SQLiteTransaction' - Inner layer for database operations
--
-- This design enforces transaction boundaries at compile time. All database
-- operations require the 'SQLiteTransaction' effect, which can only be
-- introduced via 'transact', 'transactImmediate', 'transactExclusive', or 'notransact'.
--
-- == Quick Start
--
-- @
-- import Data.Pool qualified as Pool
-- import Database.SQLite.Simple qualified as SQL
-- import Effectful
-- import Effectful.Sqlite
--
-- main :: IO ()
-- main = do
--   -- Create a connection pool
--   pool <- Pool.newPool $ Pool.defaultPoolConfig
--     (SQL.open "app.db")
--     SQL.close
--     300   -- idle timeout (seconds)
--     10    -- max connections
--
--   -- Run database operations
--   runEff . runSQLite pool $ do
--     -- Run migrations
--     runMigrations "migrations/"
--
--     -- Query data (without transaction)
--     users <- notransact $ query_ "SELECT id, name FROM users"
--     liftIO $ print users
--
--     -- Insert data (with transaction)
--     transact $ do
--       execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
--
--     -- Transactions for writes
--     transact $ do
--       execute "UPDATE accounts SET balance = balance - 100 WHERE id = ?" (Only 1)
--       execute "UPDATE accounts SET balance = balance + 100 WHERE id = ?" (Only 2)
-- @
--
-- == Handlers
--
-- Three handlers are provided for different use cases:
--
-- * 'runSQLite' - Uses a connection pool (recommended for production)
-- * 'runSQLiteWithConnection' - Uses a single connection (useful for testing)
-- * 'runSQLiteWithPath' - Auto-manages connection lifecycle (one-off scripts)
--
-- == Transaction Boundaries
--
-- * 'transact' - DEFERRED transaction (default SQLite mode)
-- * 'transactImmediate' - IMMEDIATE transaction (acquires write lock immediately)
-- * 'transactExclusive' - EXCLUSIVE transaction (prevents all other access)
-- * 'notransact' - Direct connection access without transaction (auto-commit)
--
-- == Migrations
--
-- Place SQL migration files in a directory with the naming format:
--
-- @
-- {14-digit-version}_{name}.sql
-- @
--
-- For example: @20240114120000_create_users.sql@
module Effectful.Sqlite
  ( -- * Effects
    SQLite
  , SQLiteTransaction

    -- * Handlers
  , runSQLite
  , runSQLiteWithRetry
  , runSQLiteWithConnection
  , runSQLiteWithPath

    -- * Retry Configuration
  , RetryConfig (..)
  , defaultRetryConfig

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
  , Migration (..)
  , MigrationRecord (..)
  , MigrationResult (..)
  , MigrationError (..)

    -- * Low-level
  , getRawConnection

    -- * Re-exports from sqlite-simple
  , Connection
  , Query
  , Only (..)
  , NamedParam (..)
  , (:.) (..)
    -- ** Row parsing
  , FromRow (..)
  , ToRow (..)
  , field
  , SQLData (..)
    -- ** Field parsing
  , FromField (..)
  , ToField (..)
  , Field
  , fieldData
  , ResultError (..)
  , returnError
  , Ok (..)
  )
where

import Database.SQLite.Simple (Connection, FromRow (..), NamedParam (..), Only (..), Query, SQLData (..), ToRow (..), field, (:.) (..))
import Database.SQLite.Simple.FromField (Field, FromField (..), ResultError (..), fieldData, returnError)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Effectful.Sqlite.Effect
import Effectful.Sqlite.Migration.Types
import Effectful.Sqlite.Retry (RetryConfig (..), defaultRetryConfig)
