module Effectful.Output.Dynamic
  ( -- * Effect
    Output

    -- ** Handlers
  , runOutputAction
  , runOutputLocalArray
  , runOutputLocalList
  , runOutputSharedArray
  , runOutputSharedList

    -- ** Operations
  , output
  ) where

import Data.Primitive.Array

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Output.Static.Array.Local qualified as LA
import Effectful.Output.Static.Array.Shared qualified as SA

data Output o :: Effect where
  Output :: o -> Output o m ()

type instance DispatchOf (Output o) = Dynamic

----------------------------------------
-- Handlers

runOutputAction
  :: forall o es a
   . HasCallStack
  => (HasCallStack => o -> Eff es ())
  -- ^ The action for output generation.
  -> Eff (Output o : es) a
  -> Eff es a
runOutputAction outputAction = interpret_ $ \case
  Output o -> outputAction o

runOutputLocalArray :: HasCallStack => Eff (Output o : es) a -> Eff es (a, Array o)
runOutputLocalArray = reinterpret_ LA.runOutput $ \case
  Output o -> LA.output o

runOutputLocalList :: HasCallStack => Eff (Output o : es) a -> Eff es (a, [o])
runOutputLocalList = reinterpret_ LA.runOutputList $ \case
  Output o -> LA.output o

runOutputSharedArray :: HasCallStack => Eff (Output o : es) a -> Eff es (a, Array o)
runOutputSharedArray = reinterpret_ SA.runOutput $ \case
  Output o -> SA.output o

runOutputSharedList :: HasCallStack => Eff (Output o : es) a -> Eff es (a, [o])
runOutputSharedList = reinterpret_ SA.runOutputList $ \case
  Output o -> SA.output o

----------------------------------------
-- Operations

output :: (HasCallStack, Output o :> es) => o -> Eff es ()
output = send . Output
