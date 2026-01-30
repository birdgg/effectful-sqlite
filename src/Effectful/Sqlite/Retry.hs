-- |
-- Module      : Effectful.Sqlite.Retry
-- Description : Retry logic for handling SQLite busy errors
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
-- Maintainer  : birdeggegg@gmail.com
--
-- This module provides configurable retry logic for handling SQLite
-- @ErrorBusy@ errors that occur during concurrent database access.
--
-- SQLite returns @ErrorBusy@ when the database is locked by another
-- connection. This module implements exponential backoff with random
-- jitter to handle these situations gracefully.
--
-- == Example
--
-- @
-- import Effectful.Sqlite
--
-- -- Use default retry configuration
-- runEff . runSQLiteWithRetry defaultRetryConfig pool $ do
--   execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
--
-- -- Or customize the retry behavior
-- let config = RetryConfig
--       { maxRetries = 5
--       , initialDelay = 100_000   -- 100ms
--       , backoffFactor = 2.0
--       , maxDelay = 2_000_000     -- 2s
--       }
-- runEff . runSQLiteWithRetry config pool $ do
--   execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- @
module Effectful.Sqlite.Retry
  ( -- * Retry Configuration
    RetryConfig (..)
  , defaultRetryConfig

    -- * Retry Function
  , retryOnBusy
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO, try)
import Database.SQLite3 (Error (ErrorBusy))
import Database.SQLite.Simple (SQLError (..))
import GHC.Stack (HasCallStack)
import System.Random (randomRIO)

-- | Configuration for retry behavior on SQLite busy errors.
--
-- The retry mechanism uses exponential backoff with random jitter:
--
-- * On each retry, the delay is multiplied by 'backoffFactor'
-- * A random jitter of 50-100% of the calculated delay is applied
-- * The delay is capped at 'maxDelay'
--
-- This approach helps prevent the "thundering herd" problem where
-- multiple processes retry simultaneously.
--
-- __Configuration requirements:__
--
-- * 'maxRetries' should be non-negative (0 means no retries)
-- * 'initialDelay' must be positive (at least 2 for jitter calculation)
-- * 'backoffFactor' should be >= 1.0 (values < 1.0 would decrease delays)
-- * 'maxDelay' must be positive
--
-- Use 'defaultRetryConfig' for sensible defaults.
data RetryConfig = RetryConfig
  { maxRetries :: !Int
  -- ^ Maximum number of retry attempts (default: 10)
  , initialDelay :: !Int
  -- ^ Initial delay in microseconds before first retry (default: 500000 = 0.5s)
  , backoffFactor :: !Double
  -- ^ Multiplier for delay after each retry (default: 1.5)
  , maxDelay :: !Int
  -- ^ Maximum delay in microseconds (default: 5000000 = 5s)
  }
  deriving stock (Show, Eq)

-- | Default retry configuration.
--
-- * 'maxRetries' = 10
-- * 'initialDelay' = 500,000 microseconds (0.5 seconds)
-- * 'backoffFactor' = 1.5
-- * 'maxDelay' = 5,000,000 microseconds (5 seconds)
--
-- With these defaults, the retry sequence would be approximately:
-- 0.5s, 0.75s, 1.1s, 1.7s, 2.5s, 3.8s, 5s, 5s, 5s, 5s (with random jitter)
defaultRetryConfig :: RetryConfig
defaultRetryConfig =
  RetryConfig
    { maxRetries = 10
    , initialDelay = 500_000
    , backoffFactor = 1.5
    , maxDelay = 5_000_000
    }

-- | Execute an IO action with automatic retry on SQLite busy errors.
--
-- When the action throws a 'SQLError' with 'ErrorBusy', this function
-- will retry according to the provided 'RetryConfig'. Other exceptions
-- are re-thrown immediately.
--
-- The delay between retries uses exponential backoff with random jitter
-- (50-100% of the calculated delay) to avoid synchronized retries from
-- multiple processes.
--
-- __Important:__ This function should only wrap idempotent operations.
-- It is designed for individual SQL operations, not entire transaction
-- blocks. For transaction-level retry logic, implement retry at the
-- application level.
--
-- @
-- retryOnBusy defaultRetryConfig $ do
--   SQL.execute conn "INSERT INTO users (name) VALUES (?)" (Only "Alice")
-- @
retryOnBusy :: HasCallStack => RetryConfig -> IO a -> IO a
retryOnBusy config action = go config.maxRetries config.initialDelay
  where
    go retriesLeft delay = do
      result <- try action
      case result of
        Right a -> pure a
        Left e@(SQLError sqlErr _ _)
          | sqlErr == ErrorBusy && retriesLeft > 0 -> do
              -- Random jitter: delay * [0.5, 1.0]
              jitter <- randomRIO (delay `div` 2, delay)
              threadDelay jitter
              let nextDelay =
                    min
                      config.maxDelay
                      (floor $ fromIntegral delay * config.backoffFactor)
              go (retriesLeft - 1) nextDelay
          | otherwise -> throwIO e
