{-# LANGUAGE ImplicitParams #-}
module Effectful.Output.Static.Action
  ( -- * Effect
    Output

    -- ** Handlers
  , runOutput

    -- ** Operations
  , output
  ) where

import Data.Kind
import GHC.Stack

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Utils

data Output (o :: Type) :: Effect

type instance DispatchOf (Output o) = Static NoSideEffects

-- | Wrapper to prevent a space leak on reconstruction of 'Output' in
-- 'relinkOutput' (see https://gitlab.haskell.org/ghc/ghc/-/issues/25520).
newtype OutputImpl o es where
  OutputImpl :: (HasCallStack => o -> Eff es ()) -> OutputImpl o es

data instance StaticRep (Output o) where
  Output
    :: !(Env actionEs)
    -> !(OutputImpl o actionEs)
    -> StaticRep (Output o)

runOutput
  :: forall o es a
   . HasCallStack
  => (HasCallStack => o -> Eff es ())
  -- ^ The action for output generation.
  -> Eff (Output o : es) a
  -> Eff es a
runOutput outputAction action = unsafeEff $ \es -> do
  inlineBracket
    (consEnv (Output es outputImpl) relinkOutput es)
    unconsEnv
    (unEff action)
  where
    outputImpl = OutputImpl $ let ?callStack = thawCallStack ?callStack in outputAction

output :: (HasCallStack, Output o :> es) => o -> Eff es ()
output !o = unsafeEff $ \es -> do
  Output actionEs (OutputImpl outputAction) <- getEnv es
  -- Corresponds to thawCallStack in runOutput.
  (`unEff` actionEs) $ withFrozenCallStack outputAction o

----------------------------------------
-- Helpers

relinkOutput :: Relinker StaticRep (Output o)
relinkOutput = Relinker $ \relink (Output actionEs outputAction) -> do
  newActionEs <- relink actionEs
  pure $ Output newActionEs outputAction
