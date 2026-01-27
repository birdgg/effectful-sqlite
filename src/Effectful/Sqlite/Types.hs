-- |
-- Module      : Effectful.Sqlite.Types
-- Description : Core types for SQLite operations
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
-- Maintainer  : birdeggegg@gmail.com
--
-- This module provides core types used by the SQLite effect.
module Effectful.Sqlite.Types
  ( ExecuteResult (..)
  )
where

import Data.Int (Int64)

-- | Result of an execute operation with additional metadata.
--
-- This type is returned by 'Effectful.Sqlite.executeReturning' and
-- 'Effectful.Sqlite.executeReturning_' to provide information about
-- the executed statement.
--
-- @
-- result <- executeReturning "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- liftIO $ putStrLn $ "Inserted user with ID: " <> show result.lastRowId
-- @
data ExecuteResult = ExecuteResult
  { changedRows :: Int
  -- ^ Number of rows changed by the operation.
  -- For INSERT, this is typically 1.
  -- For UPDATE\/DELETE, this is the number of affected rows.
  , lastRowId :: Int64
  -- ^ The rowid of the most recent successful INSERT into a rowid table.
  -- For tables with an INTEGER PRIMARY KEY, this is the primary key value.
  -- Returns 0 if no INSERT has been performed on this connection.
  }
  deriving stock (Show, Eq)
