-- | Support for access to a write only value of a particular type.
--
-- The value is shared between multiple threads. If you want each thead to
-- manage its own version of the value, use "Effectful.Writer.Static.Local".
--
-- /Warning:/ 'Writer'\'s state will be accumulated via __left-associated__ uses
-- of '<>', which makes it unsuitable for use with types for which such pattern
-- is inefficient. __This applies, in particular, to the standard list type__,
-- which makes the 'Writer' effect pretty niche.
--
-- __If you just want to accumulate values, use "Effectful.Output.Static.Array.Shared".__
--
-- /Note:/ while the 'Control.Monad.Trans.Writer.Strict.Writer' from the
-- @transformers@ package includes additional operations
-- 'Control.Monad.Trans.Writer.Strict.pass' and
-- 'Control.Monad.Trans.Writer.Strict.censor', they don't cooperate with runtime
-- exceptions very well, so they're deliberately omitted here.
module Effectful.Writer.Static.Shared
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

import Control.Concurrent.MVar.Strict
import Control.Exception (onException, uninterruptibleMask)
import Data.Kind

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive

-- | Provide access to a strict (WHNF), shared, write only value of type @w@.
data Writer (w :: Type) :: Effect

type instance DispatchOf (Writer w) = Static NoSideEffects
newtype instance StaticRep (Writer w) = Writer (MVar' w)

-- | Run a 'Writer' effect and return the final value along with the final
-- output.
runWriter :: (HasCallStack, Monoid w) => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = do
  v <- unsafeEff_ $ newMVar' mempty
  a <- evalStaticRep (Writer v) m
  (a, ) <$> unsafeEff_ (readMVar' v)

-- | Run a 'Writer' effect and return the final output, discarding the final
-- value.
execWriter :: (HasCallStack, Monoid w) => Eff (Writer w : es) a -> Eff es w
execWriter m = do
  v <- unsafeEff_ $ newMVar' mempty
  _ <- evalStaticRep (Writer v) m
  unsafeEff_ $ readMVar' v

-- | Append the given output to the overall output of the 'Writer'.
tell :: (HasCallStack, Writer w :> es, Monoid w) => w -> Eff es ()
tell w1 = unsafeEff $ \es -> do
  Writer v <- getEnv es
  modifyMVar'_ v $ \w0 -> let w = w0 <> w1 in pure w

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
listen m = unsafeEff $ \es -> do
  -- The mask is uninterruptible because modifyMVar_ v0 in the merge function
  -- might block and if an async exception is received while waiting, w1 will be
  -- lost.
  uninterruptibleMask $ \unmask -> do
    v1 <- newMVar' mempty
    -- Replace thread local MVar with a fresh one for isolated listening.
    v0 <- stateEnv es $ \(Writer v) -> (v, Writer v1)
    a <- unmask (unEff m es) `onException` merge es v0 v1
    (a, ) <$> merge es v0 v1
  where
    -- Merge results accumulated in the local MVar with the mainline. If an
    -- exception was received while listening, merge results recorded so far.
    merge es v0 v1 = do
      putEnv es $ Writer v0
      w1 <- readMVar' v1
      modifyMVar'_ v0 $ \w0 -> let w = w0 <> w1 in pure w
      pure w1

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
