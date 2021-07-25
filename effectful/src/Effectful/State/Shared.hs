-- | The 'State' effect.
--
-- Represented as an 'MVar' underneath, therefore:
--
-- - shareable between multiple threads,
--
-- - slower than "Effectful.State.Local".
--
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

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), shareable, mutable state of type @s@.
newtype State s :: Effect where
  State :: MVar s -> State s m r

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = do
  v <- unsafeEff_ $ newMVar s
  a <- evalEffect (IdE (State v)) m
  (a, ) <$> unsafeEff_ (readMVar v)

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s m = do
  v <- unsafeEff_ $ newMVar s
  evalEffect (IdE (State v)) m

execState :: s -> Eff (State s : es) a -> Eff es s
execState s m = do
  v <- unsafeEff_ $ newMVar s
  _ <- evalEffect (IdE (State v)) m
  unsafeEff_ $ readMVar v

get :: State s :> es => Eff es s
get = unsafeEff $ \es -> do
  IdE (State v) <- getEnv es
  readMVar v

gets :: State s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: State s :> es => s -> Eff es ()
put s = unsafeEff $ \es -> do
  IdE (State v) <- getEnv es
  modifyMVar_ v $ \_ -> s `seq` pure s

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = unsafeEff $ \es -> do
  IdE (State v) <- getEnv es
  modifyMVar v $ \s0 -> let (a, s) = f s0 in s `seq` pure (s, a)

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = unsafeEff $ \es -> do
  IdE (State v) <- getEnv es
  modifyMVar v $ \s0 -> do
    (a, s) <- unEff (f s0) es
    s `seq` pure (s, a)

modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
