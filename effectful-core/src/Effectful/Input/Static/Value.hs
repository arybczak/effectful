module Effectful.Input.Static.Value
  ( -- * Effect
    Input

    -- ** Handlers
  , runInput

    -- ** Operations
  , input
  ) where

import Data.Kind

import Effectful
import Effectful.Dispatch.Static

data Input (i :: Type) :: Effect

type instance DispatchOf (Input i) = Static NoSideEffects
newtype instance StaticRep (Input i) = Input i

runInput
  :: HasCallStack
  => i
  -- ^ The input.
  -> Eff (Input i : es) a
  -> Eff es a
runInput = evalStaticRep . Input

input :: (HasCallStack, Input i :> es) => Eff es i
input = do
  Input i <- getStaticRep
  pure i
