-- |
-- Module      : Effectful.Sqlite.Types
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
--
-- Core types for SQLite operations.
module Effectful.Sqlite.Types
  ( -- * Connection Pool
    SqlitePool (..)

    -- * Database Configuration
  , SqliteDb (..)
  )
where

import Data.Pool (Pool)
import Database.SQLite.Simple (Connection)

--------------------------------------------------------------------------------
-- Connection Pool
--------------------------------------------------------------------------------

-- | Wrapper around a connection pool.
newtype SqlitePool = SqlitePool {getPool :: Pool Connection}

--------------------------------------------------------------------------------
-- Database Configuration
--------------------------------------------------------------------------------

-- | Database connection specification.
--
-- * 'DbFile' - File path; library manages pool
-- * 'DbPool' - Existing pool for custom configuration
data SqliteDb
  = DbFile FilePath
  | DbPool SqlitePool
