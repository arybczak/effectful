-- | The 'State' effect with dynamic dispatch.
--
-- It's not clear in which situation it's beneficial to use this instead of
-- "Effectful.State" or "Effectful.State.MVar" as you either:
--
-- - Share state between threads and need the synchonized version.
--
-- - Don't share state between threads (or want it to be thread local) and are
--   free to use the faster, pure version.
--
-- However, let's include this for now.
--
module Effectful.State.Dynamic
  ( State(..)

  -- * Pure
  , runState
  , evalState
  , execState

  -- * MVar
  , runStateMVar
  , evalStateMVar
  , execStateMVar

  -- * Operations
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Effectful.Interpreter
import Effectful.Monad
import qualified Effectful.State as SP
import qualified Effectful.State.MVar as SM

-- | Provide access to a mutable state of type @s@.
--
-- Whether the state is represented as a pure value or an 'MVar' underneath
-- depends on the interpretation.
data State s :: Effect where
  Get    :: State s m s
  Put    :: ~s -> State s m ()
  State  :: (s ->   (a, s)) -> State s m a
  StateM :: (s -> m (a, s)) -> State s m a

----------------------------------------
-- Pure

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s0 = reinterpretM (SP.runState s0) statePure

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s0 = reinterpretM (SP.evalState s0) statePure

execState :: s -> Eff (State s : es) a -> Eff es s
execState s0 = reinterpretM (SP.execState s0) statePure

statePure
  :: SP.State s :> es
  => LocalEnv localEs
  -> State s (Eff localEs) a
  -> Eff es a
statePure env = \case
  Get      -> SP.get
  Put s    -> SP.put s
  State f  -> SP.state f
  StateM f -> localSeqUnlift env $ \run -> SP.stateM (run . f)

----------------------------------------
-- MVar

runStateMVar :: s -> Eff (State s : es) a -> Eff es (a, s)
runStateMVar s0 = reinterpretM (SM.runState s0) stateMVar

evalStateMVar :: s -> Eff (State s : es) a -> Eff es a
evalStateMVar s0 = reinterpretM (SM.evalState s0) stateMVar

execStateMVar :: s -> Eff (State s : es) a -> Eff es s
execStateMVar s0 = reinterpretM (SM.execState s0) stateMVar

stateMVar
  :: SM.State s :> es
  => LocalEnv localEs
  -> State s (Eff localEs) a
  -> Eff es a
stateMVar env = \case
  Get      -> SM.get
  Put s    -> SM.put s
  State f  -> SM.state f
  StateM f -> localSeqUnlift env $ \run -> SM.stateM (run . f)

----------------------------------------
-- Operations

get :: State s :> es => Eff es s
get = send Get

gets :: State s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: State s :> es => s -> Eff es ()
put = send . Put

state :: (State s :> es) => (s -> (a, s)) -> Eff es a
state = send . State

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM = send . StateM

modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
