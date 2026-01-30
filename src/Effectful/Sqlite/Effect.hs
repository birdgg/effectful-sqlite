-- |
-- Module      : Effectful.Sqlite.Effect
-- Copyright   : (c) birdgg, 2024
-- License     : MIT
--
-- Two-layer effect design for SQLite with compile-time transaction safety.
--
-- @
-- runEff . runSqlite (DbFile \"app.db\") $ do
--   users <- notransact $ query_ \"SELECT * FROM users\"
--   transact $ execute \"INSERT INTO users (name) VALUES (?)\" (Only \"Alice\")
-- @
module Effectful.Sqlite.Effect
  ( -- * Effects
    Sqlite,
    SqliteTransaction,

    -- * Handlers
    runSqlite,
    runSqliteDebug,

    -- * Connection Pool
    SqlitePool (..),
    withSqlPool,

    -- * Transaction Boundaries
    transact,
    transactImmediate,
    transactExclusive,
    notransact,

    -- * Query Operations
    query,
    query_,
    execute,
    execute_,
    executeMany,
    queryNamed,
    executeNamed,
    executeNamedReturningChanges,

    -- * Nested Transactions
    savepoint,

    -- * Migration
    runMigrations,
    getMigrationStatus,
    getPendingMigrations,

    -- * Retry
    retryBusy,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Exception (mask, onException)
import Control.Monad (when)
import Data.Function (fix, (&))
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, Error (..), FromRow, NamedParam, Query, SQLError (..), ToRow)
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Static
import Effectful.Exception qualified as E
import Effectful.Log (Log, logAttention_)
import Effectful.Sqlite.Migration.Runner qualified as M
import Effectful.Sqlite.Migration.Types
import Effectful.Sqlite.Types (SqliteDb (..), SqlitePool (..))
import System.Random (randomRIO)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Effect Definitions
--------------------------------------------------------------------------------

type SqliteLogger = String -> IO ()

noLogger :: SqliteLogger
noLogger = const (pure ())

-- | Connection pool management effect.
data Sqlite :: Effect

type instance DispatchOf Sqlite = 'Static 'WithSideEffects

data instance StaticRep Sqlite = SqliteRep SqliteLogger SqlitePool

-- | Database operations effect. Requires 'transact' or 'notransact' to introduce.
data SqliteTransaction :: Effect

type instance DispatchOf SqliteTransaction = 'Static 'NoSideEffects

data instance StaticRep SqliteTransaction = SqliteTxRep SqliteLogger Connection

--------------------------------------------------------------------------------
-- Query Operations
--------------------------------------------------------------------------------

-- | Query with parameters.
query ::
  (SqliteTransaction :> es, IOE :> es, ToRow q, FromRow r) =>
  Query ->
  q ->
  Eff es [r]
query q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.query conn q params

-- | Query without parameters.
query_ ::
  (SqliteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  Eff es [r]
query_ q = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.query_ conn q

-- | Query with named parameters.
queryNamed ::
  (SqliteTransaction :> es, IOE :> es, FromRow r) =>
  Query ->
  [NamedParam] ->
  Eff es [r]
queryNamed q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.queryNamed conn q params

--------------------------------------------------------------------------------
-- Execute Operations
--------------------------------------------------------------------------------

-- | Execute with parameters.
execute ::
  (SqliteTransaction :> es, IOE :> es, ToRow q) =>
  Query ->
  q ->
  Eff es ()
execute q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.execute conn q params

-- | Execute without parameters.
execute_ ::
  (SqliteTransaction :> es, IOE :> es) =>
  Query ->
  Eff es ()
execute_ q = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.execute_ conn q

-- | Execute with multiple parameter sets.
executeMany ::
  (SqliteTransaction :> es, IOE :> es, ToRow q) =>
  Query ->
  [q] ->
  Eff es ()
executeMany q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.executeMany conn q params

-- | Execute with named parameters.
executeNamed ::
  (SqliteTransaction :> es, IOE :> es) =>
  Query ->
  [NamedParam] ->
  Eff es ()
executeNamed q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.executeNamed conn q params

-- | Execute with named parameters, returning changed row count.
executeNamedReturningChanges ::
  (SqliteTransaction :> es, IOE :> es) =>
  Query ->
  [NamedParam] ->
  Eff es Int
executeNamedReturningChanges q params = do
  SqliteTxRep logger conn <- getStaticRep
  liftIO $ do
    logger (show q)
    SQL.executeNamed conn q params
    SQL.changes conn

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

withResourceEff :: Pool a -> (a -> Eff es r) -> Eff es r
withResourceEff pool act = unsafeEff $ \es ->
  mask $ \unmask -> do
    (resource, localPool) <- Pool.takeResource pool
    r <-
      unmask (unEff (act resource) es)
        `onException` Pool.destroyResource pool localPool resource
    Pool.putResource localPool resource
    pure r

--------------------------------------------------------------------------------
-- Retry
--------------------------------------------------------------------------------

-- | Retry on @ErrorBusy@ with exponential backoff (max 10 retries).
retryBusy ::
  (Concurrent :> es, Log :> es, IOE :> es) =>
  Eff es r ->
  Eff es r
retryBusy act =
  (10 :: Int, 0.5e6 :: Double)
    & fix
      ( \self (!left, !wait) -> do
          when (left <= 0) $
            E.throwIO $
              SQLError
                { sqlErrorDetails = "Busy retrial limit exceeded",
                  sqlErrorContext = "retryBusy",
                  sqlError = ErrorBusy
                }
          act `E.catch` \exc@SQLError {sqlError} ->
            case sqlError of
              ErrorBusy -> do
                wait' <- liftIO $ randomRIO (1e-6, wait)
                logAttention_ $
                  "ErrorBusy detected. Retrying after "
                    <> T.pack (printf "%.06f" (wait' * 1e-6))
                    <> " seconds..."
                threadDelay $ floor wait'
                self (left - 1, wait * 1.5)
              _ -> E.throwIO exc
      )

--------------------------------------------------------------------------------
-- Transaction Boundaries
--------------------------------------------------------------------------------

-- | Run without transaction (auto-commit). Retries on @ErrorBusy@.
notransact ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff (SqliteTransaction : es) a ->
  Eff es a
notransact action = do
  SqliteRep logger (SqlitePool pool) <- getStaticRep
  withResourceEff pool $ \conn ->
    evalStaticRep (SqliteTxRep logger conn) $
      retryBusy action

-- | Run in DEFERRED transaction. Retries on @ErrorBusy@.
transact ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff (SqliteTransaction : es) a ->
  Eff es a
transact = transactWith SQL.withTransaction

-- | Run in IMMEDIATE transaction (acquires write lock immediately). Retries on @ErrorBusy@.
transactImmediate ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff (SqliteTransaction : es) a ->
  Eff es a
transactImmediate = transactWith SQL.withImmediateTransaction

-- | Run in EXCLUSIVE transaction (blocks all other access). Retries on @ErrorBusy@.
transactExclusive ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  Eff (SqliteTransaction : es) a ->
  Eff es a
transactExclusive = transactWith SQL.withExclusiveTransaction

transactWith ::
  (Sqlite :> es, Concurrent :> es, Log :> es, IOE :> es) =>
  (Connection -> IO a -> IO a) ->
  Eff (SqliteTransaction : es) a ->
  Eff es a
transactWith withTx action = do
  SqliteRep logger (SqlitePool pool) <- getStaticRep
  withResourceEff pool $ \conn ->
    unsafeEff $ \es ->
      withTx conn $
        unEff (evalStaticRep (SqliteTxRep logger conn) (retryBusy action)) es

--------------------------------------------------------------------------------
-- Nested Transactions (Savepoints)
--------------------------------------------------------------------------------

-- | Run within a savepoint. On failure, only changes since savepoint are rolled back.
savepoint ::
  (SqliteTransaction :> es, IOE :> es) =>
  Eff es a ->
  Eff es a
savepoint action = do
  SqliteTxRep _logger conn <- getStaticRep
  unsafeEff $ \es -> SQL.withSavepoint conn $ unEff action es

--------------------------------------------------------------------------------
-- Connection Pool
--------------------------------------------------------------------------------

-- | Create a connection pool with default settings (0.5s idle timeout).
withSqlPool ::
  (IOE :> es) =>
  FilePath ->
  (SqlitePool -> Eff es a) ->
  Eff es a
withSqlPool dbPath =
  E.bracket
    ( liftIO $ do
        num <- getNumCapabilities
        fmap SqlitePool $
          Pool.newPool $
            Pool.defaultPoolConfig
              ( do
                  conn <- SQL.open dbPath
                  SQL.execute_ conn "PRAGMA busy_timeout=3000;"
                  pure conn
              )
              SQL.close
              0.5
              num
    )
    (liftIO . Pool.destroyAllResources . getPool)

--------------------------------------------------------------------------------
-- Migration Operations
--------------------------------------------------------------------------------

-- | Run pending migrations from directory. Files: @{version}_{name}.sql@
runMigrations ::
  (Sqlite :> es, IOE :> es) =>
  FilePath ->
  Eff es (Either MigrationError MigrationResult)
runMigrations dir = do
  SqliteRep _logger (SqlitePool pool) <- getStaticRep
  withResourceEff pool $ \conn ->
    liftIO $ M.runMigrations conn dir

-- | Get executed and pending migrations.
getMigrationStatus ::
  (Sqlite :> es, IOE :> es) =>
  FilePath ->
  Eff es (Either MigrationError ([MigrationRecord], [Migration]))
getMigrationStatus dir = do
  SqliteRep _logger (SqlitePool pool) <- getStaticRep
  withResourceEff pool $ \conn ->
    liftIO $ M.getMigrationStatus conn dir

-- | Get pending migrations only.
getPendingMigrations ::
  (Sqlite :> es, IOE :> es) =>
  FilePath ->
  Eff es (Either MigrationError [Migration])
getPendingMigrations dir = do
  SqliteRep _logger (SqlitePool pool) <- getStaticRep
  withResourceEff pool $ \conn ->
    liftIO $ M.getPendingMigrations conn dir

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Run Sqlite effect without logging.
runSqlite ::
  (IOE :> es) =>
  SqliteDb ->
  Eff (Sqlite : es) a ->
  Eff es a
runSqlite (DbFile dbPath) act =
  withSqlPool dbPath $ \pool ->
    evalStaticRep (SqliteRep noLogger pool) act
runSqlite (DbPool pool) act =
  evalStaticRep (SqliteRep noLogger pool) act
{-# INLINE runSqlite #-}

-- | Run Sqlite effect with SQL logging.
runSqliteDebug ::
  (IOE :> es) =>
  (String -> IO ()) ->
  SqliteDb ->
  Eff (Sqlite : es) a ->
  Eff es a
runSqliteDebug logger (DbFile dbPath) act =
  withSqlPool dbPath $ \pool ->
    evalStaticRep (SqliteRep logger pool) act
runSqliteDebug logger (DbPool pool) act =
  evalStaticRep (SqliteRep logger pool) act
{-# INLINE runSqliteDebug #-}
