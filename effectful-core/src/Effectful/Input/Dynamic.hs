module Effectful.Input.Dynamic
  ( -- * Effect
    Input

    -- ** Handlers
  , runInputAction
  , runInputValue

    -- ** Operations
  , input
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

data Input i :: Effect where
  Input :: Input i m i

type instance DispatchOf (Input i) = Dynamic

----------------------------------------
-- Handlers

runInputAction
  :: forall i es a
   . HasCallStack
  => (HasCallStack => Eff es i)
  -- ^ The action for input generation.
  -> Eff (Input i : es) a
  -> Eff es a
runInputAction inputAction = interpret_ $ \case
  Input -> inputAction

runInputValue
  :: HasCallStack
  => i
  -- ^ The input value.
  -> Eff (Input i : es) a
  -> Eff es a
runInputValue inputValue = interpret_ $ \case
  Input -> pure inputValue

----------------------------------------
-- Operations

input :: (HasCallStack, Input i :> es) => Eff es i
input = send Input
