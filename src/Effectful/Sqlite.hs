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
-- * 'Sqlite' - Outer layer for connection pool management
-- * 'SqliteTransaction' - Inner layer for database operations
--
-- This design enforces transaction boundaries at compile time. All database
-- operations require the 'SqliteTransaction' effect, which can only be
-- introduced via 'transact', 'transactImmediate', 'transactExclusive', or 'notransact'.
--
-- == Quick Start
--
-- @
-- import Effectful
-- import Effectful.Sqlite
--
-- main :: IO ()
-- main = runEff . runLog "app" logger . runConcurrent . runSqlite (DbFile "app.db") $ do
--   -- Run migrations
--   runMigrations "migrations/"
--
--   -- Query data (without transaction)
--   users <- notransact $ query_ "SELECT id, name FROM users"
--   liftIO $ print users
--
--   -- Insert data (with transaction)
--   transact $ do
--     execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- @
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
    Sqlite
  , SqliteTransaction

    -- * Handlers
  , runSqlite
  , runSqliteDebug

    -- * Connection Pool
  , SqlitePool (..)
  , withSqlPool

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

    -- * Database Configuration
  , SqliteDb (..)

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

    -- * Retry
  , retryBusy

    -- * Re-exports from effectful
  , Concurrent
  , runConcurrent
  , Log
  , runLog

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
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Log (Log, runLog)
import Effectful.Sqlite.Effect
import Effectful.Sqlite.Migration.Types
import Effectful.Sqlite.Types (SqliteDb (..))
