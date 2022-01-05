-- | Support for access to a mutable value of a particular type, which is:
--
-- - shared between multiple threads (if you want each thead to manage its own
--   version of the value, see "Effectful.State.Local").
--
-- - slightly slower than "Effectful.State.Local".
--
-- /Note:/ unlike the 'Control.Monad.Trans.State.StateT' monad transformer from
-- the @transformers@ library, the 'State' effect doesn't lose state
-- modifications when an exception is received:
--
-- >>> import qualified Control.Monad.Trans.State.Strict as S
--
-- >>> :{
--   (`S.execStateT` "Hi") . handle (\(_::ErrorCall) -> pure ()) $ do
--     S.modify (++ " there!")
--     error "oops"
-- :}
-- "Hi"
--
-- >>> :{
--   runEff . execState "Hi" . handle (\(_::ErrorCall) -> pure ()) $ do
--     modify (++ " there!")
--     error "oops"
-- :}
-- "Hi there!"
module Effectful.State.Shared
  ( State
  , runState
  , evalState
  , execState
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Control.Concurrent.MVar

import Effectful.Dispatch.Static
import Effectful.Monad

-- | Provide access to a strict (WHNF), shared, mutable value of type @s@.
newtype State s :: Effect where
  State :: MVar s -> State s m r

type instance EffectStyle (State s) = DataA

-- | Run a 'State' effect with the given initial state and return the final
-- value along with the final state.
runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = do
  v <- unsafeEff_ $ newMVar s
  a <- evalData (DataA (State v)) m
  (a, ) <$> unsafeEff_ (readMVar v)

-- | Run a 'State' effect with the given initial state and return the final
-- value, discarding the final state.
evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s m = do
  v <- unsafeEff_ $ newMVar s
  evalData (DataA (State v)) m

-- | Run a 'State' effect with the given initial state and return the final
-- state, discarding the final value.
execState :: s -> Eff (State s : es) a -> Eff es s
execState s m = do
  v <- unsafeEff_ $ newMVar s
  _ <- evalData (DataA (State v)) m
  unsafeEff_ $ readMVar v

-- | Fetch the current value of the state.
get :: State s :> es => Eff es s
get = unsafeEff $ \es -> do
  DataA (State v) <- getEnv es
  readMVar v

-- | Get a function of the current state.
--
-- @'gets' f ≡ f '<$>' 'get'@
gets :: State s :> es => (s -> a) -> Eff es a
gets f = f <$> get

-- | Set the current state to the given value.
put :: State s :> es => s -> Eff es ()
put s = unsafeEff $ \es -> do
  DataA (State v) <- getEnv es
  modifyMVar_ v $ \_ -> s `seq` pure s

-- | Apply the function to the current state and return a value.
--
-- /Note:/ this function gets an exclusive access to the state for its duration.
state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = unsafeEff $ \es -> do
  DataA (State v) <- getEnv es
  modifyMVar v $ \s0 -> let (a, s) = f s0 in s `seq` pure (s, a)

-- | Apply the function to the current state.
--
-- @'modify' f ≡ 'state' (\\s -> ((), f s))@
--
-- /Note:/ this function gets an exclusive access to the state for its duration.
modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

-- | Apply the monadic function to the current state and return a value.
--
-- /Note:/ this function gets an exclusive access to the state for its duration.
stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = unsafeEff $ \es -> do
  DataA (State v) <- getEnv es
  modifyMVar v $ \s0 -> do
    (a, s) <- unEff (f s0) es
    s `seq` pure (s, a)

-- | Apply the monadic function to the current state.
--
-- @'modifyM' f ≡ 'stateM' (\\s -> ((), ) '<$>' f s)@
--
-- /Note:/ this function gets an exclusive access to the state for its duration.
modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)

-- $setup
-- >>> import Control.Exception (ErrorCall)
-- >>> import Control.Monad.Catch
