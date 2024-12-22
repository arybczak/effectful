{-# LANGUAGE ImplicitParams #-}
module Effectful.Input.Static.Action
  ( -- * Effect
    Input

    -- ** Handlers
  , runInput

    -- ** Operations
  , input
  ) where

import Data.Kind
import GHC.Stack

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Utils

data Input (i :: Type) :: Effect

type instance DispatchOf (Input i) = Static NoSideEffects

-- | Wrapper to prevent a space leak on reconstruction of 'Input' in
-- 'relinkInput' (see https://gitlab.haskell.org/ghc/ghc/-/issues/25520).
newtype InputImpl i es where
  InputImpl :: (HasCallStack => Eff es i) -> InputImpl i es

data instance StaticRep (Input i) where
  Input
    :: !(Env inputEs)
    -> !(InputImpl i inputEs)
    -> StaticRep (Input i)

runInput
  :: forall i es a
   . HasCallStack
  => (HasCallStack => Eff es i)
  -- ^ The action for input generation.
  -> Eff (Input i : es) a
  -> Eff es a
runInput inputAction action = unsafeEff $ \es -> do
  inlineBracket
    (consEnv (Input es inputImpl) relinkInput es)
    unconsEnv
    (unEff action)
  where
    inputImpl = InputImpl $ let ?callStack = thawCallStack ?callStack in inputAction

input :: (HasCallStack, Input i :> es) => Eff es i
input = unsafeEff $ \es -> do
  Input inputEs (InputImpl inputAction) <- getEnv es
  -- Corresponds to thawCallStack in runInput.
  (`unEff` inputEs) $ withFrozenCallStack inputAction

----------------------------------------
-- Helpers

relinkInput :: Relinker StaticRep (Input i)
relinkInput = Relinker $ \relink (Input inputEs inputAction) -> do
  newActionEs <- relink inputEs
  pure $ Input newActionEs inputAction
