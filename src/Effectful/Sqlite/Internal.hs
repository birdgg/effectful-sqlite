module Effectful.Sqlite.Internal
  ( TxContext
  , withPoolConn
  , unliftWithPoolConn
  , withSingleConn
  , unliftWithSingleConn
  )
where

import Control.Exception (bracket_)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Database.SQLite.Simple (Connection)
import Effectful
import Effectful.Dispatch.Dynamic (LocalEnv, localSeqUnliftIO)

-- | Transaction context: Nothing = use pool, Just = use this connection
type TxContext = IORef (Maybe Connection)


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
    Just conn -> f conn
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
  localSeqUnliftIO env $ \unlift ->
    Pool.withResource pool $ \conn ->
      bracket_
        (writeIORef txCtx (Just conn))
        (writeIORef txCtx Nothing)
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
