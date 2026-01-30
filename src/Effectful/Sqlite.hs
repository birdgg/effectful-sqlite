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
--     -- Query data
--     users <- query_ "SELECT id, name FROM users"
--     liftIO $ print users
--
--     -- Insert data
--     execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
--
--     -- Transactions
--     withTransaction $ do
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
  ( -- * Effect
    SQLite

    -- * Handlers
  , runSQLite
  , runSQLiteWithRetry
  , runSQLiteWithConnection
  , runSQLiteWithPath

    -- * Retry Configuration
  , RetryConfig (..)
  , defaultRetryConfig

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
  , Migration (..)
  , MigrationRecord (..)
  , MigrationResult (..)
  , MigrationError (..)

    -- * Low-level
  , withConnection

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
import Database.SQLite.Simple.FromField (FromField (..), Field, fieldData, ResultError (..), returnError)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Effectful.Sqlite.Effect
import Effectful.Sqlite.Migration.Types
import Effectful.Sqlite.Retry (RetryConfig (..), defaultRetryConfig)
