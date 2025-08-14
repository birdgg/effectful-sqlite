{-# LANGUAGE TemplateHaskell #-}

module Effectful.SQLite.Connection where

import qualified Database.SQLite.Simple as SQLite
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

data WithConnection :: Effect where
  WithConnection :: (SQLite.Connection -> m a) -> WithConnection m a

makeEffect ''WithConnection

runWithConnection ::
  (HasCallStack) => SQLite.Connection -> Eff (WithConnection : es) a -> Eff es a
runWithConnection conn = interpret $ \env -> \case
  WithConnection f ->
    localSeqUnlift env $ \unlift -> unlift $ f conn
