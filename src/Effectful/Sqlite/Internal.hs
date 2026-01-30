-- | Internal module for connection management.
--
-- This module is not part of the public API and may change without notice.
module Effectful.Sqlite.Internal
  ( -- * Pool Helpers
    withResourceEff
  )
where

import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)

-- | Run an action with a resource from a pool in the Eff monad.
--
-- This is a lifted version of 'Pool.withResource' for use with effectful.
-- The resource is automatically returned to the pool when the action completes,
-- even if an exception is thrown.
withResourceEff ::
  (IOE :> es) =>
  Pool a ->
  (a -> Eff es b) ->
  Eff es b
withResourceEff pool f = do
  (resource, localPool) <- unsafeEff_ $ Pool.takeResource pool
  result <- f resource
  unsafeEff_ $ Pool.putResource localPool resource
  pure result
