{-# LANGUAGE TemplateHaskell #-}

module Effectful.Sqlite.Effect
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

    -- * Low-level
  , withConnection
  )
where

import Data.IORef (newIORef)
import Data.Pool (Pool)
import Database.SQLite.Simple (Connection, FromRow, NamedParam, Query, ToRow)
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Sqlite.Internal (TxContext, unliftWithPoolConn, unliftWithSingleConn, withPoolConn, withSingleConn)
import Effectful.Sqlite.Migration.Runner qualified as M
import Effectful.Sqlite.Migration.Types
import Effectful.Sqlite.Types (ExecuteResult (..))
import Effectful.TH

-- | SQLite database effect
data SQLite :: Effect where
  Query :: (ToRow q, FromRow r) => Query -> q -> SQLite m [r]
  Query_ :: (FromRow r) => Query -> SQLite m [r]
  Execute :: (ToRow q) => Query -> q -> SQLite m ()
  Execute_ :: Query -> SQLite m ()
  ExecuteMany :: (ToRow q) => Query -> [q] -> SQLite m ()
  QueryNamed :: (FromRow r) => Query -> [NamedParam] -> SQLite m [r]
  ExecuteNamed :: Query -> [NamedParam] -> SQLite m ()
  ExecuteNamedReturningChanges :: Query -> [NamedParam] -> SQLite m Int
  ExecuteReturning :: (ToRow q) => Query -> q -> SQLite m ExecuteResult
  ExecuteReturning_ :: Query -> SQLite m ExecuteResult
  -- Streaming operations
  Fold :: (ToRow q, FromRow r) => Query -> q -> a -> (a -> r -> m a) -> SQLite m a
  Fold_ :: (FromRow r) => Query -> a -> (a -> r -> m a) -> SQLite m a
  FoldNamed :: (FromRow r) => Query -> [NamedParam] -> a -> (a -> r -> m a) -> SQLite m a
  -- Transaction
  WithTransaction :: m a -> SQLite m a
  WithSavepoint :: m a -> SQLite m a
  WithImmediateTransaction :: m a -> SQLite m a
  WithExclusiveTransaction :: m a -> SQLite m a
  Begin :: SQLite m ()
  Commit :: SQLite m ()
  Rollback :: SQLite m ()
  -- Migration
  RunMigrations :: FilePath -> SQLite m (Either MigrationError MigrationResult)
  GetMigrationStatus :: FilePath -> SQLite m (Either MigrationError ([MigrationRecord], [Migration]))
  GetPendingMigrations :: FilePath -> SQLite m (Either MigrationError [Migration])
  CreateMigrationsTable :: SQLite m ()
  -- Low-level
  WithConnection :: (Connection -> m a) -> SQLite m a

type instance DispatchOf SQLite = Dynamic

makeEffect ''SQLite

-- | Run SQLite effect using a connection pool
runSQLite ::
  (HasCallStack, IOE :> es) =>
  Pool Connection ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLite pool action = do
  txCtx <- liftIO $ newIORef Nothing
  interpret (handleSQLite pool txCtx) action

-- | Run SQLite effect using a single connection (no pool)
--
-- Useful for testing or short-lived operations where connection pooling
-- is not needed.
runSQLiteWithConnection ::
  (HasCallStack, IOE :> es) =>
  Connection ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLiteWithConnection conn = interpret (handleSQLiteWithConnection conn)

-- | Run SQLite effect using a database path, automatically managing the connection
--
-- Opens a connection at the start and closes it when done.
-- Useful for one-off scripts or simple applications.
runSQLiteWithPath ::
  (HasCallStack, IOE :> es) =>
  FilePath ->
  Eff (SQLite : es) a ->
  Eff es a
runSQLiteWithPath dbPath action = do
  conn <- liftIO $ SQL.open dbPath
  result <- runSQLiteWithConnection conn action
  liftIO $ SQL.close conn
  pure result

-- | SQLite effect handler
handleSQLite ::
  (HasCallStack, IOE :> es) =>
  Pool Connection ->
  TxContext ->
  EffectHandler SQLite es
handleSQLite pool txCtx env = \case
  Query q params ->
    withPoolConn pool txCtx $ \conn -> SQL.query conn q params
  Query_ q ->
    withPoolConn pool txCtx $ \conn -> SQL.query_ conn q
  Execute q params ->
    withPoolConn pool txCtx $ \conn -> SQL.execute conn q params
  Execute_ q ->
    withPoolConn pool txCtx $ \conn -> SQL.execute_ conn q
  ExecuteMany q params ->
    withPoolConn pool txCtx $ \conn -> SQL.executeMany conn q params
  QueryNamed q params ->
    withPoolConn pool txCtx $ \conn -> SQL.queryNamed conn q params
  ExecuteNamed q params ->
    withPoolConn pool txCtx $ \conn -> SQL.executeNamed conn q params
  ExecuteNamedReturningChanges q params ->
    withPoolConn pool txCtx $ \conn -> do
      SQL.executeNamed conn q params
      SQL.changes conn
  ExecuteReturning q params ->
    withPoolConn pool txCtx $ \conn -> do
      SQL.execute conn q params
      changed <- SQL.changes conn
      rowId <- SQL.lastInsertRowId conn
      pure ExecuteResult {changedRows = changed, lastRowId = rowId}
  ExecuteReturning_ q ->
    withPoolConn pool txCtx $ \conn -> do
      SQL.execute_ conn q
      changed <- SQL.changes conn
      rowId <- SQL.lastInsertRowId conn
      pure ExecuteResult {changedRows = changed, lastRowId = rowId}
  Fold q params acc f ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      SQL.fold conn q params acc (\a r -> unlift (f a r))
  Fold_ q acc f ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      SQL.fold_ conn q acc (\a r -> unlift (f a r))
  FoldNamed q params acc f ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      SQL.foldNamed conn q params acc (\a r -> unlift (f a r))
  WithTransaction innerAction ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      SQL.withTransaction conn (unlift innerAction)
  WithSavepoint innerAction ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      SQL.withSavepoint conn (unlift innerAction)
  WithImmediateTransaction innerAction ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      SQL.withImmediateTransaction conn (unlift innerAction)
  WithExclusiveTransaction innerAction ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      SQL.withExclusiveTransaction conn (unlift innerAction)
  Begin ->
    withPoolConn pool txCtx $ \conn -> SQL.execute_ conn "BEGIN"
  Commit ->
    withPoolConn pool txCtx $ \conn -> SQL.execute_ conn "COMMIT"
  Rollback ->
    withPoolConn pool txCtx $ \conn -> SQL.execute_ conn "ROLLBACK"
  RunMigrations dir ->
    withPoolConn pool txCtx $ \conn -> M.runMigrations conn dir
  GetMigrationStatus dir ->
    withPoolConn pool txCtx $ \conn -> M.getMigrationStatus conn dir
  GetPendingMigrations dir ->
    withPoolConn pool txCtx $ \conn -> M.getPendingMigrations conn dir
  CreateMigrationsTable ->
    withPoolConn pool txCtx M.createMigrationsTable
  WithConnection f ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      unlift (f conn)

-- | Execute an action for each row in the result set
forEach ::
  (HasCallStack, SQLite :> es, ToRow q, FromRow r) =>
  Query ->
  q ->
  (r -> Eff es ()) ->
  Eff es ()
forEach q params f = fold q params () (\_ r -> f r)

-- | Execute an action for each row in the result set (no parameters)
forEach_ ::
  (HasCallStack, SQLite :> es, FromRow r) =>
  Query ->
  (r -> Eff es ()) ->
  Eff es ()
forEach_ q f = fold_ q () (\_ r -> f r)

-- | Execute an action for each row in the result set (named parameters)
forEachNamed ::
  (HasCallStack, SQLite :> es, FromRow r) =>
  Query ->
  [NamedParam] ->
  (r -> Eff es ()) ->
  Eff es ()
forEachNamed q params f = foldNamed q params () (\_ r -> f r)

-- | SQLite effect handler for single connection mode
handleSQLiteWithConnection ::
  (HasCallStack, IOE :> es) =>
  Connection ->
  EffectHandler SQLite es
handleSQLiteWithConnection conn env = \case
  Query q params ->
    withSingleConn conn $ \c -> SQL.query c q params
  Query_ q ->
    withSingleConn conn $ \c -> SQL.query_ c q
  Execute q params ->
    withSingleConn conn $ \c -> SQL.execute c q params
  Execute_ q ->
    withSingleConn conn $ \c -> SQL.execute_ c q
  ExecuteMany q params ->
    withSingleConn conn $ \c -> SQL.executeMany c q params
  QueryNamed q params ->
    withSingleConn conn $ \c -> SQL.queryNamed c q params
  ExecuteNamed q params ->
    withSingleConn conn $ \c -> SQL.executeNamed c q params
  ExecuteNamedReturningChanges q params ->
    withSingleConn conn $ \c -> do
      SQL.executeNamed c q params
      SQL.changes c
  ExecuteReturning q params ->
    withSingleConn conn $ \c -> do
      SQL.execute c q params
      changed <- SQL.changes c
      rowId <- SQL.lastInsertRowId c
      pure ExecuteResult {changedRows = changed, lastRowId = rowId}
  ExecuteReturning_ q ->
    withSingleConn conn $ \c -> do
      SQL.execute_ c q
      changed <- SQL.changes c
      rowId <- SQL.lastInsertRowId c
      pure ExecuteResult {changedRows = changed, lastRowId = rowId}
  Fold q params acc f ->
    unliftWithSingleConn conn env $ \c unlift ->
      SQL.fold c q params acc (\a r -> unlift (f a r))
  Fold_ q acc f ->
    unliftWithSingleConn conn env $ \c unlift ->
      SQL.fold_ c q acc (\a r -> unlift (f a r))
  FoldNamed q params acc f ->
    unliftWithSingleConn conn env $ \c unlift ->
      SQL.foldNamed c q params acc (\a r -> unlift (f a r))
  WithTransaction innerAction ->
    unliftWithSingleConn conn env $ \c unlift ->
      SQL.withTransaction c (unlift innerAction)
  WithSavepoint innerAction ->
    unliftWithSingleConn conn env $ \c unlift ->
      SQL.withSavepoint c (unlift innerAction)
  WithImmediateTransaction innerAction ->
    unliftWithSingleConn conn env $ \c unlift ->
      SQL.withImmediateTransaction c (unlift innerAction)
  WithExclusiveTransaction innerAction ->
    unliftWithSingleConn conn env $ \c unlift ->
      SQL.withExclusiveTransaction c (unlift innerAction)
  Begin ->
    withSingleConn conn $ \c -> SQL.execute_ c "BEGIN"
  Commit ->
    withSingleConn conn $ \c -> SQL.execute_ c "COMMIT"
  Rollback ->
    withSingleConn conn $ \c -> SQL.execute_ c "ROLLBACK"
  RunMigrations dir ->
    withSingleConn conn $ \c -> M.runMigrations c dir
  GetMigrationStatus dir ->
    withSingleConn conn $ \c -> M.getMigrationStatus c dir
  GetPendingMigrations dir ->
    withSingleConn conn $ \c -> M.getPendingMigrations c dir
  CreateMigrationsTable ->
    withSingleConn conn M.createMigrationsTable
  WithConnection f ->
    unliftWithSingleConn conn env $ \c unlift ->
      unlift (f c)
