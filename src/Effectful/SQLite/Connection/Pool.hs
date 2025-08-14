module Effectful.SQLite.Connection.Pool (
  runWithConnectionPool,
  module Pool,
)
where

import qualified Database.SQLite.Simple as SQLite
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.SQLite.Connection
import UnliftIO.Pool as Pool

{- | Rather than keeping one connection alive and re-using it for the whole
process, we might want to create a 'Pool' of connections and only "ask" for
one when we need it. This function uses "UnliftIO.Pool" to do just that.
-}
runWithConnectionPool ::
  (HasCallStack, IOE :> es) =>
  Pool.Pool SQLite.Connection ->
  Eff (WithConnection : es) a ->
  Eff es a
runWithConnectionPool pool = interpret $ \env -> \case
  WithConnection f ->
    localSeqUnlift env $ \unlift -> do
      Pool.withResource pool $ unlift . f
