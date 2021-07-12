module Effectful.Monad
  ( -- * The 'Eff' monad
    Eff
  , runPureEff

  -- ** Effect constraints
  , Effect
  , (:>)

  -- * Arbitrary I/O
  , IOE
  , runEff

  -- * Primitive actions
  , Prim
  , runPrim

  -- ** Unlift strategies
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Monad
