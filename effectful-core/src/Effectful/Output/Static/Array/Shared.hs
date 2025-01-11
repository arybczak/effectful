module Effectful.Output.Static.Array.Shared
  ( -- * Effect
    Output

    -- ** Handlers
  , runOutput
  , runOutputList

    -- ** Operations
  , output

    -- * Re-exports
  , Array
  ) where

import Control.Concurrent.MVar.Strict
import Control.Monad.Primitive
import Data.Foldable qualified as F
import Data.Kind
import Data.Primitive.Array

import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Utils
import Effectful.Internal.Env

data Output (o :: Type) :: Effect

data OutputData o = OutputData !Int !(MutableArray RealWorld o)

type instance DispatchOf (Output o) = Static NoSideEffects
newtype instance StaticRep (Output o) = Output (MVar' (OutputData o))

runOutput :: HasCallStack => Eff (Output o : es) a -> Eff es (a, Array o)
runOutput = runOutputImpl $ \(OutputData size arr) -> do
  freezeArray arr 0 size

runOutputList :: HasCallStack => Eff (Output o : es) a -> Eff es (a, [o])
runOutputList = runOutputImpl $ \(OutputData size arr) -> do
  take size . F.toList <$> unsafeFreezeArray arr

output :: (HasCallStack, Output o :> es) => o -> Eff es ()
output !o = unsafeEff $ \es -> do
  Output v <- getEnv es
  modifyMVar'_ v $ \(OutputData size arr0) -> do
    let len0 = sizeofMutableArray arr0
    arr <- case size `compare` len0 of
      GT -> error $ "size (" ++ show size ++ ") > len0 (" ++ show len0 ++ ")"
      LT -> pure arr0
      EQ -> do
        let len = growCapacity len0
        arr <- newArray len undefinedValue
        copyMutableArray arr 0 arr0 0 size
        pure arr
    writeArray arr size o
    pure $ OutputData (size + 1) arr

----------------------------------------
-- Helpers

runOutputImpl
  :: HasCallStack
  => (OutputData o -> IO acc)
  -> Eff (Output o : es) a
  -> Eff es (a, acc)
runOutputImpl f action = do
  v <- unsafeEff_ $ newMVar' . OutputData 0 =<< newArray 0 undefinedValue
  a <- evalStaticRep (Output v) action
  acc <- unsafeEff_ $ f =<< readMVar' v
  pure (a, acc)

undefinedValue :: HasCallStack => a
undefinedValue = error "Undefined value"
