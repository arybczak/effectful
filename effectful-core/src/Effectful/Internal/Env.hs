{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
module Effectful.Internal.Env
  ( -- * The environment
    Env(..)
  , Storage(..)

    -- ** Relinker
  , Relinker(..)
  , dummyRelinker

    -- * Dispatch
  , Dispatch(..)
  , SideEffects(..)
  , DispatchOf
  , EffectRep

    -- * Operations
  , emptyEnv
  , cloneEnv
  , restoreEnv
  , sizeEnv
  , tailEnv

    -- ** Modification of the effect stack
  , consEnv
  , unconsEnv
  , replaceEnv
  , unreplaceEnv
  , subsumeEnv
  , injectEnv

    -- ** Data retrieval and update
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Primitive
import Data.IORef.Strict
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import GHC.Stack (HasCallStack)

import Effectful.Internal.Effect
import Effectful.Internal.Utils

type role Env nominal

-- | A strict (WHNF), __thread local__, mutable, extensible record indexed by types
-- of kind 'Effect'.
--
-- __Warning: the environment is a mutable data structure and cannot be simultaneously used from multiple threads under any circumstances.__
--
-- In order to pass it to a different thread, you need to perform a deep copy
-- with the 'cloneEnv' funtion.
--
-- Offers very good performance characteristics for most often performed
-- operations:
--
-- - Extending: /@O(n)@/, where @n@ is the size of the effect stack.
--
-- - Shrinking: /@O(1)@/.
--
-- - Indexing via '(:>)': /@O(1)@/
--
-- - Modification of a specific element: /@O(1)@/.
--
-- - Getting a tail: /@O(1)@/.
--
-- - Cloning: /@O(N)@/, where @N@ is the size of the 'Storage'.
--
data Env (es :: [Effect]) = Env
  { envOffset  :: !Int
  , envRefs    :: !(PrimArray Int)
  , envStorage :: !(IORef' Storage)
  }

-- | A storage of effects.
data Storage = Storage
  { stOwnerId   :: !Int
  , stSize      :: !Int
  , stVersion   :: !Int
  , stVersions  :: !(MutablePrimArray RealWorld Int)
  , stEffects   :: !(SmallMutableArray RealWorld Any)
  , stRelinkers :: !(SmallMutableArray RealWorld Any)
  }

----------------------------------------
-- Relinker

-- | A function for relinking 'Env' objects stored in the handlers and/or making
-- a deep copy of the representation of the effect when cloning the environment.
newtype Relinker :: (Effect -> Type) -> Effect -> Type where
  Relinker
    :: ((forall es. Env es -> IO (Env es)) -> rep e -> IO (rep e))
    -> Relinker rep e

-- | A dummy 'Relinker'.
dummyRelinker :: Relinker rep e
dummyRelinker = Relinker $ \_ -> pure

----------------------------------------
-- Dispatch

-- | A type of dispatch. For more information consult the documentation in
-- "Effectful.Dispatch.Dynamic" and "Effectful.Dispatch.Static".
data Dispatch = Dynamic | Static SideEffects

-- | Signifies whether core operations of a statically dispatched effect perform
-- side effects. If an effect is marked as such, the
-- 'Effectful.Dispatch.Static.runStaticRep' family of functions will require the
-- 'Effectful.IOE' effect to be in context via the
-- 'Effectful.Dispatch.Static.MaybeIOE' type family.
data SideEffects = NoSideEffects | WithSideEffects

-- | Dispatch types of effects.
type family DispatchOf (e :: Effect) :: Dispatch

-- | Internal representations of effects.
type family EffectRep (d :: Dispatch) :: Effect -> Type

----------------------------------------
-- Operations

-- | Create an empty environment.
emptyEnv :: HasCallStack => IO (Env '[])
emptyEnv = Env 0
  <$> (unsafeFreezePrimArray =<< newPrimArray 0)
  <*> (newIORef' =<< emptyStorage)

-- | Clone the environment to use it in a different thread.
cloneEnv :: Env es -> IO (Env es)
cloneEnv (Env offset refs storage0) = do
  Storage _ storageSize version vs0 es0 fs0 <- readIORef' storage0
  vsSize <- getSizeofMutablePrimArray  vs0
  esSize <- getSizeofSmallMutableArray es0
  fsSize <- getSizeofSmallMutableArray fs0
  when (vsSize /= esSize) $ do
    error $ "vsSize (" ++ show vsSize ++ ") /= esSize (" ++ show esSize ++ ")"
  when (esSize /= fsSize) $ do
    error $ "esSize (" ++ show esSize ++ ") /= fsSize (" ++ show fsSize ++ ")"
  vs <- cloneMutablePrimArray  vs0 0 vsSize
  es <- cloneSmallMutableArray es0 0 esSize
  fs <- cloneSmallMutableArray fs0 0 fsSize
  storage <- newIORef' $ Storage (-1) storageSize version vs es fs
  let relinkEffects = \case
        0 -> pure ()
        k -> do
          let i = k - 1
          Relinker f <- fromAny <$> readSmallArray fs i
          readSmallArray es i
            >>= f (relinkEnv storage) . fromAny
            >>= writeSmallArray' es i . toAny
          relinkEffects i
  relinkEffects storageSize
  pure $ Env offset refs storage
{-# NOINLINE cloneEnv #-}

-- | Restore the environment from its clone.
--
-- @since 2.2.0.0
restoreEnv
  :: Env es -- ^ Destination.
  -> Env es -- ^ Source.
  -> IO ()
restoreEnv dest src = do
  destStorage <- readStorage (envStorage dest)
  srcStorage  <- readStorage (envStorage src)
  let destStorageSize = stSize destStorage
      srcStorageSize  = stSize srcStorage
  when (destStorageSize /= srcStorageSize) $ do
    error $ "destStorageSize (" ++ show destStorageSize
         ++ ") /= srcStorageSize (" ++ show srcStorageSize ++ ")"
  writeIORef' (envStorage dest) $ srcStorage
    -- Decreasing the counter allows leakage of unsafeCoerce (see unsafeCoerce2
    -- in the EnvTests module).
    { stVersion = max (stVersion destStorage) (stVersion srcStorage)
    }
{-# NOINLINE restoreEnv #-}

-- | Get the current size of the environment.
sizeEnv :: Env es -> IO Int
sizeEnv (Env offset refs _) = do
  pure $ (sizeofPrimArray refs - offset) `div` 2

-- | Access the tail of the environment.
tailEnv :: Env (e : es) -> IO (Env es)
tailEnv (Env offset refs storage) = do
  pure $ Env (offset + 2) refs storage

----------------------------------------
-- Extending and shrinking

-- | Extend the environment with a new data type.
consEnv
  :: HasCallStack
  => EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> Env es
  -> IO (Env (e : es))
consEnv e f (Env offset refs0 storage) = do
  let size = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray (size + 2)
  copyPrimArray mrefs 2 refs0 offset size
  (ref, version) <- insertEffect storage e f
  writePrimArray mrefs 0 ref
  writePrimArray mrefs 1 version
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE consEnv #-}

-- | Shrink the environment by one data type.
--
-- /Note:/ after calling this function @e@ from the input environment is no
-- longer usable.
unconsEnv :: HasCallStack => Env (e : es) -> IO ()
unconsEnv (Env _ refs storage) = do
  deleteEffect storage (indexPrimArray refs 0)
{-# NOINLINE unconsEnv #-}

----------------------------------------

-- | Replace a specific effect in the stack with a new value.
--
-- /Note:/ unlike in 'putEnv' the value in not changed in place, so only the new
-- environment will see it.
replaceEnv
  :: forall e es. (HasCallStack, e :> es)
  => EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> Env es
  -> IO (Env es)
replaceEnv e f (Env offset refs0 storage) = do
  let size = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray size
  copyPrimArray mrefs 0 refs0 offset size
  (ref, version) <- insertEffect storage e f
  let i = 2 * reifyIndex @e @es
  writePrimArray mrefs  i      ref
  writePrimArray mrefs (i + 1) version
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE replaceEnv #-}

-- | Remove a reference to the replaced effect.
--
-- /Note:/ after calling this function the input environment is no longer
-- usable.
unreplaceEnv :: forall e es. (HasCallStack, e :> es) => Env es -> IO ()
unreplaceEnv (Env offset refs storage) = do
  deleteEffect storage $ indexPrimArray refs (offset + 2 * reifyIndex @e @es)
{-# NOINLINE unreplaceEnv #-}

----------------------------------------

-- | Reference an existing effect from the top of the stack.
subsumeEnv :: forall e es. e :> es => Env es -> IO (Env (e : es))
subsumeEnv (Env offset refs0 storage) = do
  let size = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray (size + 2)
  copyPrimArray mrefs 2 refs0 offset size
  let ix = offset + 2 * reifyIndex @e @es
  writePrimArray mrefs 0 $ indexPrimArray refs0  ix
  writePrimArray mrefs 1 $ indexPrimArray refs0 (ix + 1)
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE subsumeEnv #-}

----------------------------------------

-- | Construct an environment containing a permutation (with possible
-- duplicates) of a subset of effects from the input environment.
injectEnv :: forall xs es. Subset xs es => Env es -> IO (Env xs)
injectEnv (Env offset refs0 storage) = do
  let xs         = reifyIndices @xs @es
      permSize   = 2 * length xs
      prefixSize = 2 * prefixLength @es
      suffixSize = if subsetFullyKnown @xs @es
                   then 0
                   else sizeofPrimArray refs0 - offset - prefixSize
  mrefs <- newPrimArray (permSize + suffixSize)
  copyPrimArray mrefs permSize refs0 (offset + prefixSize) suffixSize
  let writePermRefs i = \case
        []       -> pure ()
        (e : es) -> do
          let ix = offset + 2 * e
          writePrimArray mrefs  i      $ indexPrimArray refs0  ix
          writePrimArray mrefs (i + 1) $ indexPrimArray refs0 (ix + 1)
          writePermRefs (i + 2) es
  writePermRefs 0 xs
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE injectEnv #-}

----------------------------------------
-- Data retrieval and update

-- | Extract a specific data type from the environment.
getEnv
  :: forall e es. (HasCallStack, e :> es)
  => Env es -- ^ The environment.
  -> IO (EffectRep (DispatchOf e) e)
getEnv env = do
  (i, es) <- getLocation @e env
  fromAny <$> readSmallArray es i

-- | Replace the data type in the environment with a new value (in place).
putEnv
  :: forall e es. (HasCallStack, e :> es)
  => Env es -- ^ The environment.
  -> EffectRep (DispatchOf e) e
  -> IO ()
putEnv env e = do
  (i, es) <- getLocation @e env
  writeSmallArray' es i (toAny e)

-- | Modify the data type in the environment and return a value (in place).
stateEnv
  :: forall e es a. (HasCallStack, e :> es)
  => Env es -- ^ The environment.
  -> (EffectRep (DispatchOf e) e -> (a, EffectRep (DispatchOf e) e))
  -> IO a
stateEnv env f = do
  (i, es) <- getLocation @e env
  (a, e) <- f . fromAny <$> readSmallArray es i
  writeSmallArray' es i (toAny e)
  pure a

-- | Modify the data type in the environment (in place).
modifyEnv
  :: forall e es. (HasCallStack, e :> es)
  => Env es -- ^ The environment.
  -> (EffectRep (DispatchOf e) e -> (EffectRep (DispatchOf e) e))
  -> IO ()
modifyEnv env f = do
  (i, es) <- getLocation @e env
  e <- f . fromAny <$> readSmallArray es i
  writeSmallArray' es i (toAny e)

-- | Determine location of the effect in the environment.
getLocation
  :: forall e es. (HasCallStack, e :> es)
  => Env es
  -> IO (Int, SmallMutableArray RealWorld Any)
getLocation (Env offset refs storage) = do
  let i       = offset + 2 * reifyIndex @e @es
      ref     = indexPrimArray refs  i
      version = indexPrimArray refs (i + 1)
  Storage _ _ _ vs es _ <- readStorage storage
  storageVersion <- readPrimArray vs ref
  -- If version of the reference is different than version in the storage, it
  -- means that the effect in the storage is not the one that was initially
  -- referenced.
  when (version /= storageVersion) $ do
    error $ "version (" ++ show version ++ ") /= storageVersion ("
         ++ show storageVersion ++ ")"
  pure (ref, es)

----------------------------------------
-- Internal helpers

-- | Create an empty storage.
emptyStorage :: HasCallStack => IO Storage
emptyStorage = Storage (-1) 0 (noVersion + 1)
  <$> newPrimArray 0
  <*> newSmallArray 0 undefinedData
  <*> newSmallArray 0 undefinedData

-- | Insert an effect into the storage and return its reference.
insertEffect
  :: HasCallStack
  => IORef' Storage
  -> EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> IO (Int, Int)
insertEffect storage e f = do
  Storage tid size version vs0 es0 fs0 <- readStorage storage
  len0 <- getSizeofSmallMutableArray es0
  case size `compare` len0 of
    GT -> error $ "size (" ++ show size ++ ") > len0 (" ++ show len0 ++ ")"
    LT -> do
      writePrimArray   vs0 size version
      writeSmallArray' es0 size (toAny e)
      writeSmallArray' fs0 size (toAny f)
      writeIORef' storage $ Storage tid (size + 1) (version + 1) vs0 es0 fs0
      pure (size, version)
    EQ -> do
      let len = doubleCapacity len0
      vs <- newPrimArray len
      es <- newSmallArray len undefinedData
      fs <- newSmallArray len undefinedData
      copyMutablePrimArray  vs 0 vs0 0 size
      copySmallMutableArray es 0 es0 0 size
      copySmallMutableArray fs 0 fs0 0 size
      writePrimArray   vs size version
      writeSmallArray' es size (toAny e)
      writeSmallArray' fs size (toAny f)
      writeIORef' storage $ Storage tid (size + 1) (version + 1) vs es fs
      pure (size, version)

-- | Given a reference to an effect from the top of the stack, delete it from
-- the storage.
deleteEffect :: HasCallStack => IORef' Storage -> Int -> IO ()
deleteEffect storage ref = do
  Storage tid size version vs es fs <- readStorage storage
  when (ref /= size - 1) $ do
    error $ "ref (" ++ show ref ++ ") /= size - 1 (" ++ show (size - 1) ++ ")"
  writePrimArray  vs ref noVersion
  writeSmallArray es ref undefinedData
  writeSmallArray fs ref undefinedData
  writeIORef' storage $ Storage tid (size - 1) version vs es fs

-- | Relink the environment to use the new storage.
relinkEnv :: IORef' Storage -> Env es -> IO (Env es)
relinkEnv storage (Env offset refs _) = pure $ Env offset refs storage

-- | Double the capacity of an array.
doubleCapacity :: Int -> Int
doubleCapacity n = max 1 n * 2

noVersion :: Int
noVersion = 0

undefinedData :: HasCallStack => a
undefinedData = error "undefined data"

-- | A strict version of 'writeSmallArray'.
writeSmallArray' :: SmallMutableArray RealWorld a -> Int -> a -> IO ()
writeSmallArray' arr i a = a `seq` writeSmallArray arr i a

#if !MIN_VERSION_primitive(0,9,0)
getSizeofSmallMutableArray :: SmallMutableArray RealWorld a -> IO Int
getSizeofSmallMutableArray arr = pure $! sizeofSmallMutableArray arr
#endif

readStorage :: HasCallStack => IORef' Storage -> IO Storage
readStorage ref = do
  storage <- readIORef' ref
  tid <- myThreadId
  if stOwnerId storage == -1
    then do
      let boundStorage = storage { stOwnerId = weakThreadId tid }
      writeIORef' ref boundStorage
      pure boundStorage
    else do
      if stOwnerId storage == weakThreadId tid
        then pure storage
        else error $ "Storage used from multiple threads: owner ("
               ++ show (stOwnerId storage) ++ ") /= current ("
               ++ show (weakThreadId tid) ++ ")"
