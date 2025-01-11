-- | Support for access to a write only value of a particular type.
--
-- The value is thread local. If you want it to be shared between threads, use
-- "Effectful.Writer.Static.Shared".
--
-- /Warning:/ 'Writer'\'s state will be accumulated via __left-associated__ uses
-- of '<>', which makes it unsuitable for use with types for which such pattern
-- is inefficient. __This applies, in particular, to the standard list type__,
-- which makes the 'Writer' effect pretty niche.
--
-- __If you just want to accumulate values, use "Effectful.Output.Static.Array.Local".__
--
-- /Note:/ while the 'Control.Monad.Trans.Writer.Strict.Writer' from the
-- @transformers@ package includes additional operations
-- 'Control.Monad.Trans.Writer.Strict.pass' and
-- 'Control.Monad.Trans.Writer.Strict.censor', they don't cooperate with runtime
-- exceptions very well, so they're deliberately omitted here.
module Effectful.Writer.Static.Local
  ( -- * Effect
    Writer

    -- ** Handlers
  , runWriter
  , execWriter

    -- ** Operations
  , tell
  , listen
  , listens
  ) where

import Control.Exception (onException, mask)
import Data.Kind

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive

-- | Provide access to a strict (WHNF), thread local, write only value of type
-- @w@.
data Writer (w :: Type) :: Effect

type instance DispatchOf (Writer w) = Static NoSideEffects
newtype instance StaticRep (Writer w) = Writer w

-- | Run a 'Writer' effect and return the final value along with the final
-- output.
runWriter :: (HasCallStack, Monoid w) => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = do
  (a, Writer w) <- runStaticRep (Writer mempty) m
  pure (a, w)

-- | Run a 'Writer' effect and return the final output, discarding the final
-- value.
execWriter :: (HasCallStack, Monoid w) => Eff (Writer w : es) a -> Eff es w
execWriter m = do
  Writer w <- execStaticRep (Writer mempty) m
  pure w

-- | Append the given output to the overall output of the 'Writer'.
tell :: (HasCallStack, Writer w :> es, Monoid w) => w -> Eff es ()
tell w = stateStaticRep $ \(Writer w0) -> ((), Writer (w0 <> w))

-- | Execute an action and append its output to the overall output of the
-- 'Writer'.
--
-- /Note:/ if an exception is received while the action is executed, the partial
-- output of the action will still be appended to the overall output of the
-- 'Writer':
--
-- >>> :{
--   runEff . execWriter @String $ do
--     tell "Hi"
--     handle (\(_::ErrorCall) -> pure ((), "")) $ do
--       tell " there"
--       listen $ do
--         tell "!"
--         error "oops"
-- :}
-- "Hi there!"
listen :: (HasCallStack, Writer w :> es, Monoid w) => Eff es a -> Eff es (a, w)
listen m = unsafeEff $ \es -> mask $ \unmask -> do
  w0 <- stateEnv es $ \(Writer w) -> (w, Writer mempty)
  a <- unmask (unEff m es) `onException` merge es w0
  (a, ) <$> merge es w0
  where
    merge es w0 =
      -- If an exception is thrown, restore w0 and keep parts of w1.
      stateEnv es $ \(Writer w1) -> (w1, Writer (w0 <> w1))

-- | Execute an action and append its output to the overall output of the
-- 'Writer', then return the final value along with a function of the recorded
-- output.
--
-- @'listens' f m ≡ 'Data.Bifunctor.second' f '<$>' 'listen' m@
listens
  :: (HasCallStack, Writer w :> es, Monoid w)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)

-- $setup
-- >>> import Control.Exception (ErrorCall)
-- >>> import Effectful.Exception
