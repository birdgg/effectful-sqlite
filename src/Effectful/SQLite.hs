{-# LANGUAGE CPP #-}

module Effectful.SQLite (
  -- * Effect
  WithConnection,
  withConnection,

  -- ** Interpreters
  runWithConnection,

  -- ** Queries that return results
  query,
  query_,
  queryWith,
  queryWith_,

  -- ** Statements that do not return results
  execute,
  execute_,
)
where

import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.FromRow as SQLite
import Effectful
import Effectful.SQLite.Connection as Conn
import GHC.Stack

-- | Lifted 'SQLite.query'.
query ::
  (HasCallStack, WithConnection :> es, IOE :> es, SQLite.ToRow q, SQLite.FromRow r) =>
  SQLite.Query ->
  q ->
  Eff es [r]
query q row = withConnection $ \conn -> liftIO (SQLite.query conn q row)

-- | Lifted 'SQLite.query_'.
query_ ::
  (HasCallStack, WithConnection :> es, IOE :> es, SQLite.FromRow r) =>
  SQLite.Query ->
  Eff es [r]
query_ row = withConnection $ \conn -> liftIO (SQLite.query_ conn row)

-- | Lifted 'SQLite.queryWith'.
queryWith ::
  (HasCallStack, WithConnection :> es, IOE :> es, SQLite.ToRow q) =>
  SQLite.RowParser r ->
  SQLite.Query ->
  q ->
  Eff es [r]
queryWith parser q row =
  withConnection $ \conn -> liftIO (SQLite.queryWith parser conn q row)

-- | Lifted 'SQLite.queryWith_'.
queryWith_ ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  SQLite.RowParser r ->
  SQLite.Query ->
  Eff es [r]
queryWith_ parser row =
  withConnection $ \conn -> liftIO (SQLite.queryWith_ parser conn row)

-- | Lifted 'SQLite.execute'.
execute ::
  (HasCallStack, WithConnection :> es, IOE :> es, SQLite.ToRow q) =>
  SQLite.Query ->
  q ->
  Eff es ()
execute q row = withConnection $ \conn -> liftIO (SQLite.execute conn q row)

-- | Lifted 'SQLite.execute_'.
execute_ ::
  (HasCallStack, WithConnection :> es, IOE :> es) =>
  SQLite.Query ->
  Eff es ()
execute_ row = withConnection $ \conn -> liftIO (SQLite.execute_ conn row)
