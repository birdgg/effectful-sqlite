-- |
-- Module      : Effectful.Sqlite.Migration.Types
-- Description : Types for database migrations
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
-- Maintainer  : birdeggegg@gmail.com
--
-- This module provides types for the database migration system.
--
-- == Migration File Format
--
-- Migration files must follow the naming convention:
--
-- @
-- {14-digit-version}_{name}.sql
-- @
--
-- For example:
--
-- @
-- 20240114120000_create_users.sql
-- 20240114130000_add_email_to_users.sql
-- @
--
-- The version is typically a timestamp in @YYYYMMDDHHmmss@ format.
module Effectful.Sqlite.Migration.Types
  ( -- * Migration Types
    Migration (..)
  , MigrationRecord (..)

    -- * Result Types
  , MigrationResult (..)
  , MigrationError (..)
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)

-- | A migration file parsed from the filesystem.
--
-- Migrations are loaded from a directory and executed in version order.
data Migration = Migration
  { version :: Text
  -- ^ Version identifier from filename (e.g., @"20240114120000"@).
  -- Must be exactly 14 digits.
  , name :: Text
  -- ^ Descriptive name from filename (e.g., @"create_users"@).
  -- Used for display purposes and stored in the migrations table.
  , sql :: ByteString
  -- ^ Raw SQL content of the migration file.
  -- Can contain multiple statements separated by semicolons.
  }
  deriving stock (Show, Eq)

-- | A record of an executed migration stored in the @_migrations@ table.
--
-- This is used to track which migrations have already been applied
-- to the database.
data MigrationRecord = MigrationRecord
  { version :: Text
  -- ^ The version identifier of the executed migration.
  , name :: Text
  -- ^ The name of the executed migration.
  , executedAt :: UTCTime
  -- ^ Timestamp when the migration was executed.
  }
  deriving stock (Show, Eq)

instance FromRow MigrationRecord where
  fromRow = MigrationRecord <$> field <*> field <*> field

-- | Result of running migrations.
data MigrationResult
  = MigrationSuccess Int
  -- ^ Successfully executed N migrations.
  | MigrationNoOp
  -- ^ No pending migrations to run.
  deriving stock (Show, Eq)

-- | Errors that can occur during migration operations.
data MigrationError
  = InvalidFilename FilePath Text
  -- ^ Invalid migration filename format.
  -- The first field is the filename, the second is the reason.
  --
  -- Expected format: @{14-digit-version}_{name}.sql@
  -- e.g., @20240114120000_create_users.sql@
  | ReadError FilePath Text
  -- ^ Failed to read migration file.
  -- The first field is the file path, the second is the error message.
  | SqlError Text Text
  -- ^ SQL execution error during migration.
  -- The first field is a description, the second is the SQLite error message.
  | DirectoryNotFound FilePath
  -- ^ The specified migrations directory does not exist.
  deriving stock (Show, Eq)
