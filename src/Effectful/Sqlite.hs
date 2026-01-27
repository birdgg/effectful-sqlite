-- | Effectful bindings for SQLite with connection pooling and migrations
module Effectful.Sqlite
  ( -- * Effect
    SQLite

    -- * Handlers
  , runSQLite
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
    -- ** Row parsing
  , FromRow (..)
  , ToRow (..)
  , field
  , SQLData (..)
  )
where

import Database.SQLite.Simple (Connection, FromRow (..), NamedParam (..), Only (..), Query, SQLData (..), ToRow (..), field)
import Effectful.Sqlite.Effect
import Effectful.Sqlite.Migration.Types
