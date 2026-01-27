module Effectful.Sqlite.Types
  ( ExecuteResult (..)
  )
where

import Data.Int (Int64)

-- | Result of an execute operation with additional information
data ExecuteResult = ExecuteResult
  { changedRows :: Int
  -- ^ Number of rows changed by the operation
  , lastRowId :: Int64
  -- ^ The rowid of the most recent successful INSERT
  }
  deriving stock (Show, Eq)
