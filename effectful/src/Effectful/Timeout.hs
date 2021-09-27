module Effectful.Timeout
  ( Timeout
  , runTimeout
  , timeout
  ) where

import qualified System.Timeout as T

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | An effect for timing out computations.
data Timeout :: Effect where
  Timeout :: Timeout m r

-- | Run the 'Timeout' effect.
runTimeout :: IOE :> es => Eff (Timeout : es) a -> Eff es a
runTimeout = evalEffect (IdE Timeout)

-- | Lifted 'T.timeout'.
timeout
  :: Timeout :> es
  => Int
  -- ^ The timeout in microseconds (1/10^6 seconds).
  -> Eff es a
  -- ^ The computation the timeout applies to.
  -> Eff es (Maybe a)
timeout n k = unsafeEff $ \es -> do
  seqUnliftEff es $ \unlift -> do
    T.timeout n $ unlift k
