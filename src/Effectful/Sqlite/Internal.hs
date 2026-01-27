-- | Internal module for connection management.
--
-- This module is not part of the public API and may change without notice.
module Effectful.Sqlite.Internal
  ( -- * Connection Strategy
    ConnStrategy (..)
  , poolStrategy
  , singleConnStrategy
  , strategyWithConn
  , strategyUnliftWithConn
  , strategyBegin
  , strategyEndTx

    -- * Transaction Context
  , TxContext
  , withPoolConn
  , unliftWithPoolConn
  , withSingleConn
  , unliftWithSingleConn
  , beginManualTx
  , endManualTx
  )
where

import Control.Exception (bracket_)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Dispatch.Dynamic (LocalEnv, localSeqUnliftIO)

-- | Transaction context for connection pooling.
--
-- When 'Nothing', operations acquire a fresh connection from the pool.
-- When 'Just', operations reuse the stored connection (used during transactions
-- to ensure all operations use the same connection).
type TxContext = IORef (Maybe (Connection, Pool.LocalPool Connection))

-- | Abstract connection strategy for SQLite operations.
--
-- This allows the handler to be parameterized over different connection
-- management approaches (pooled vs single connection).
--
-- Note: We use explicit accessor functions instead of record syntax
-- because the fields contain higher-rank types that don't work well
-- with OverloadedRecordDot.
data ConnStrategy es = ConnStrategy
  { _runWithConn :: forall a. (Connection -> IO a) -> Eff es a
  -- ^ Run an IO action with a connection
  , _runUnliftWithConn :: forall localEs a. LocalEnv localEs es -> (Connection -> (forall r. Eff localEs r -> IO r) -> IO a) -> Eff es a
  -- ^ Run an effectful action with a connection and unlift capability
  , _runBegin :: Eff es ()
  -- ^ Begin a manual transaction
  , _runEndTx :: Eff es ()
  -- ^ End a manual transaction (commit or rollback)
  }

-- | Run an IO action with a connection from the strategy.
strategyWithConn :: ConnStrategy es -> (Connection -> IO a) -> Eff es a
strategyWithConn (ConnStrategy f _ _ _) = f
{-# INLINE strategyWithConn #-}

-- | Run an effectful action with a connection and unlift capability.
strategyUnliftWithConn :: ConnStrategy es -> LocalEnv localEs es -> (Connection -> (forall r. Eff localEs r -> IO r) -> IO a) -> Eff es a
strategyUnliftWithConn (ConnStrategy _ f _ _) = f
{-# INLINE strategyUnliftWithConn #-}

-- | Begin a manual transaction.
strategyBegin :: ConnStrategy es -> Eff es ()
strategyBegin (ConnStrategy _ _ b _) = b
{-# INLINE strategyBegin #-}

-- | End a manual transaction.
strategyEndTx :: ConnStrategy es -> Eff es ()
strategyEndTx (ConnStrategy _ _ _ e) = e
{-# INLINE strategyEndTx #-}

-- | Create a connection strategy using a connection pool.
--
-- This strategy:
--
-- * Acquires connections from the pool for each operation
-- * Reuses the same connection within a transaction context
-- * Properly releases connections back to the pool
poolStrategy ::
  (IOE :> es) =>
  Pool Connection ->
  TxContext ->
  ConnStrategy es
poolStrategy pool txCtx =
  ConnStrategy
    { _runWithConn = withPoolConn pool txCtx
    , _runUnliftWithConn = unliftWithPoolConn pool txCtx
    , _runBegin = do
        conn <- beginManualTx pool txCtx
        liftIO $ SQL.execute_ conn "BEGIN"
    , _runEndTx = endManualTx pool txCtx
    }

-- | Create a connection strategy using a single connection.
--
-- This strategy:
--
-- * Always uses the provided connection
-- * Does not manage connection lifecycle
-- * Suitable for testing or when connection pooling is not needed
singleConnStrategy ::
  (IOE :> es) =>
  Connection ->
  ConnStrategy es
singleConnStrategy conn =
  ConnStrategy
    { _runWithConn = withSingleConn conn
    , _runUnliftWithConn = unliftWithSingleConn conn
    , _runBegin = liftIO $ SQL.execute_ conn "BEGIN"
    , _runEndTx = pure ()
    }


-- | Run an IO action with a connection from the pool or the current transaction
withPoolConn ::
  (IOE :> es) =>
  Pool Connection ->
  TxContext ->
  (Connection -> IO a) ->
  Eff es a
withPoolConn pool txCtx f = liftIO $ do
  mConn <- readIORef txCtx
  case mConn of
    Just (conn, _localPool) -> f conn
    Nothing -> Pool.withResource pool f

-- | Run an effectful action with a dedicated transaction connection
unliftWithPoolConn ::
  (IOE :> es) =>
  Pool Connection ->
  TxContext ->
  LocalEnv localEs es ->
  (Connection -> (forall r. Eff localEs r -> IO r) -> IO a) ->
  Eff es a
unliftWithPoolConn pool txCtx env f =
  localSeqUnliftIO env $ \unlift -> do
    (conn, localPool) <- Pool.takeResource pool
    bracket_
      (writeIORef txCtx (Just (conn, localPool)))
      (do
        writeIORef txCtx Nothing
        Pool.putResource localPool conn)
      (f conn unlift)

-- | Run an IO action with a single connection (no pool)
withSingleConn ::
  (IOE :> es) =>
  Connection ->
  (Connection -> IO a) ->
  Eff es a
withSingleConn conn f = liftIO $ f conn

-- | Run an effectful action with a single connection (no pool)
unliftWithSingleConn ::
  (IOE :> es) =>
  Connection ->
  LocalEnv localEs es ->
  (Connection -> (forall r. Eff localEs r -> IO r) -> IO a) ->
  Eff es a
unliftWithSingleConn conn env f =
  localSeqUnliftIO env $ \unlift -> f conn unlift

-- | Begin a manual transaction by acquiring a connection from the pool
-- and storing it in TxContext. Returns the connection for use with BEGIN.
beginManualTx ::
  (IOE :> es) =>
  Pool Connection ->
  TxContext ->
  Eff es Connection
beginManualTx pool txCtx = liftIO $ do
  mConn <- readIORef txCtx
  case mConn of
    Just (conn, _) -> pure conn -- Already in a transaction, reuse connection
    Nothing -> do
      (conn, localPool) <- Pool.takeResource pool
      writeIORef txCtx (Just (conn, localPool))
      pure conn

-- | End a manual transaction by releasing the connection back to the pool
-- and clearing TxContext.
endManualTx ::
  (IOE :> es) =>
  Pool Connection ->
  TxContext ->
  Eff es ()
endManualTx _pool txCtx = liftIO $ do
  mConn <- readIORef txCtx
  case mConn of
    Nothing -> pure () -- No connection to release
    Just (conn, localPool) -> do
      writeIORef txCtx Nothing
      Pool.putResource localPool conn
