{-# LANGUAGE UndecidableInstances #-}
-- | Dynamically dispatched effects.
module Effectful.Dispatch.Dynamic
  ( -- * Introduction
    -- $intro

    -- ** An example
    -- $example

    -- ** First order and higher order effects
    -- $order

    -- ** Integration with @mtl@ style effects
    -- $integration

    -- * Sending operations to the handler
    send

    -- * Handling effects
  , EffectHandler
  , interpret
  , reinterpret
  , interpose
  , impose

    -- ** Handling local 'Eff' computations
  , LocalEnv

    -- *** Unlifts
  , localSeqUnlift
  , localSeqUnliftIO
  , localUnlift
  , localUnliftIO

    -- *** Lifts
  , withLiftMap
  , withLiftMapIO

    -- *** Bidirectional lifts
  , localLiftUnlift
  , localLiftUnliftIO

    -- *** Utils
  , SharedSuffix

    -- * Re-exports
  , HasCallStack
  ) where

import Control.Exception (bracket)
import Control.Monad.IO.Unlift
import GHC.Stack (HasCallStack)
import GHC.TypeLits

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- $intro
--
-- A dynamically dispatched effect is a collection of operations that can be
-- interpreted in different ways at runtime, depending on the handler that is
-- used to run the effect.
--
-- This allows a programmer to separate the __what__ from the __how__,
-- i.e. define effects that model what the code should do, while providing
-- handlers that determine how it should do it later. Moreover, different
-- environments can use different handlers to change the behavior of specific
-- parts of the application if appropriate.
--

-- $example
--
-- Let's create an effect for basic file access, i.e. writing and reading files.
--
-- First, we need to define a generalized algebraic data type of kind 'Effect',
-- where each constructor corresponds to a specific operation of the effect in
-- question.
--
-- >>> :{
--   data FileSystem :: Effect where
--     ReadFile  :: FilePath -> FileSystem m String
--     WriteFile :: FilePath -> String -> FileSystem m ()
-- :}
--
-- >>> type instance DispatchOf FileSystem = Dynamic
--
-- The @FileSystem@ effect has two operations:
--
-- - @ReadFile@, which takes a @FilePath@ and returns a @String@ in the monadic
--   context.
--
-- - @WriteFile@, which takes a @FilePath@, a @String@ and returns a @()@ in the
--   monadic context.
--
-- For people familiar with @mtl@ style effects, note that the syntax looks very
-- similar to defining an appropriate type class:
--
-- @
-- class FileSystem m where
--   readFile  :: FilePath -> m String
--   writeFile :: FilePath -> String -> m ()
-- @
--
-- The biggest difference between these two is that the definition of a type
-- class gives us operations as functions, while the definition of an effect
-- gives us operations as data constructors. They can be turned into functions
-- with the help of 'send':
--
-- >>> :{
--   readFile :: (HasCallStack, FileSystem :> es) => FilePath -> Eff es String
--   readFile path = send (ReadFile path)
-- :}
--
-- >>> :{
--   writeFile :: (HasCallStack, FileSystem :> es) => FilePath -> String -> Eff es ()
--   writeFile path content = send (WriteFile path content)
-- :}
--
-- /Note:/ the above functions and the 'DispatchOf' instance can also be
-- automatically generated by the
-- [@makeEffect@](https://hackage.haskell.org/package/effectful-th/docs/Effectful-TH.html#v:makeEffect)
-- function from the
-- [effectful-th](https://hackage.haskell.org/package/effectful-th) package.
--
-- The following defines an 'EffectHandler' that reads and writes files from the
-- drive:
--
-- >>> import Control.Exception (IOException)
-- >>> import Control.Monad.Catch (catch)
-- >>> import qualified System.IO as IO
--
-- >>> import Effectful.Error.Static
--
-- >>> newtype FsError = FsError String deriving Show
--
-- >>> :{
--  runFileSystemIO
--    :: (IOE :> es, Error FsError :> es)
--    => Eff (FileSystem : es) a
--    -> Eff es a
--  runFileSystemIO = interpret $ \_ -> \case
--    ReadFile path           -> adapt $ IO.readFile path
--    WriteFile path contents -> adapt $ IO.writeFile path contents
--    where
--      adapt m = liftIO m `catch` \(e::IOException) -> throwError . FsError $ show e
-- :}
--
-- Here, we use 'interpret' and simply execute corresponding 'IO' actions for
-- each operation, additionally doing a bit of error management.
--
-- On the other hand, maybe there is a situation in which instead of interacting
-- with the outside world, a pure, in-memory storage is preferred:
--
-- >>> import qualified Data.Map.Strict as M
--
-- >>> import Effectful.State.Static.Local
--
-- >>> :{
--   runFileSystemPure
--     :: Error FsError :> es
--     => M.Map FilePath String
--     -> Eff (FileSystem : es) a
--     -> Eff es a
--   runFileSystemPure fs0 = reinterpret (evalState fs0) $ \_ -> \case
--     ReadFile path -> gets (M.lookup path) >>= \case
--       Just contents -> pure contents
--       Nothing       -> throwError . FsError $ "File not found: " ++ show path
--     WriteFile path contents -> modify $ M.insert path contents
-- :}
--
-- Here, we use 'reinterpret' and introduce a
-- t'Effectful.State.Static.Local.State' effect for the storage that is private
-- to the effect handler and cannot be accessed outside of it.
--
-- Let's compare how these differ.
--
-- >>> :{
--   action = do
--     file <- readFile "effectful-core.cabal"
--     pure $ length file > 0
-- :}
--
-- >>> :t action
-- action :: (FileSystem :> es) => Eff es Bool
--
-- >>> runEff . runError @FsError . runFileSystemIO $ action
-- Right True
--
-- >>> runPureEff . runErrorNoCallStack @FsError . runFileSystemPure M.empty $ action
-- Left (FsError "File not found: \"effectful-core.cabal\"")
--

-- $order
--
-- Note that the definition of the @FileSystem@ effect from the previous section
-- doesn't use the @m@ type parameter. What is more, when the effect is
-- interpreted, the 'LocalEnv' argument of the 'EffectHandler' is also not
-- used. Such effects are /first order/.
--
-- If an effect makes use of the @m@ parameter, it is a /higher order effect/.
--
-- Interpretation of higher order effects is slightly more involving. To see
-- why, let's consider the @Profiling@ effect for logging how much time a
-- specific action took to run:
--
-- >>> :{
--   data Profiling :: Effect where
--     Profile :: String -> m a -> Profiling m a
-- :}
--
-- >>> type instance DispatchOf Profiling = Dynamic
--
-- >>> :{
--   profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
--   profile label action = send (Profile label action)
-- :}
--
-- If we naively try to interpret it, we will run into trouble:
--
-- >>> import GHC.Clock (getMonotonicTime)
--
-- >>> :{
--  runProfiling :: IOE :> es => Eff (Profiling : es) a -> Eff es a
--  runProfiling = interpret $ \_ -> \case
--    Profile label action -> do
--      t1 <- liftIO getMonotonicTime
--      r <- action
--      t2 <- liftIO getMonotonicTime
--      liftIO . putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
--      pure r
-- :}
-- ...
-- ... Couldn't match type ‘localEs’ with ‘es’
-- ...
--
-- The problem is that @action@ has a type @Eff localEs a@, while the monad of
-- the effect handler is @Eff es@. @localEs@ represents the /local environment/
-- in which the @Profile@ operation was called, which is opaque as the effect
-- handler cannot possibly know how it looks like.
--
-- The solution is to use the 'LocalEnv' that an 'EffectHandler' is given to run
-- the action using one of the functions from the 'localUnlift' family:
--
-- >>> :{
--  runProfiling :: IOE :> es => Eff (Profiling : es) a -> Eff es a
--  runProfiling = interpret $ \env -> \case
--    Profile label action -> localSeqUnliftIO env $ \unlift -> do
--      t1 <- getMonotonicTime
--      r <- unlift action
--      t2 <- getMonotonicTime
--      putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
--      pure r
-- :}
--
-- In a similar way we can define a dummy interpreter that does no profiling:
--
-- >>> :{
--  runNoProfiling :: Eff (Profiling : es) a -> Eff es a
--  runNoProfiling = interpret $ \env -> \case
--    Profile label action -> localSeqUnlift env $ \unlift -> unlift action
-- :}
--
-- ...and it's done.
--
-- >>> action = profile "greet" . liftIO $ putStrLn "Hello!"
--
-- >>> :t action
-- action :: (Profiling :> es, IOE :> es) => Eff es ()
--
-- >>> runEff . runProfiling $ action
-- Hello!
-- Action 'greet' took ... seconds.
--
-- >>> runEff . runNoProfiling $ action
-- Hello!
--

-- $integration
--
-- #integration#
--
-- There exists a lot of libraries that provide their functionality as an @mtl@
-- style effect, which generally speaking is a type class that contains core
-- operations of the library in question.
--
-- Such effects are quite easy to use with the 'Eff' monad. As an example,
-- consider the @mtl@ style effect for generation of random numbers:
--
-- >>> :{
--   class Monad m => MonadRNG m where
--     randomInt :: m Int
-- :}
--
-- Let's say the library also defines a helper function for generation of random
-- strings:
--
-- >>> import Control.Monad
-- >>> import Data.Char
--
-- >>> :{
--  randomString :: MonadRNG m => Int -> m String
--  randomString n = map chr <$> replicateM n randomInt
-- :}
--
-- To make it possible to use it with the 'Eff' monad, the first step is to
-- create an effect with operations that mirror the ones of a type class:
--
-- >>> :{
--   data RNG :: Effect where
--     RandomInt :: RNG m Int
-- :}
--
-- >>> type instance DispatchOf RNG = Dynamic
--
-- If we continued as in the example above, we'd now create top level helper
-- functions that execute effect operations using 'send', in this case
-- @randomInt@ tied to @RandomInt@. But this function is already declared by the
-- @MonadRNG@ type class! Therefore, what we do instead is provide an
-- __orphan__, __canonical__ instance of @MonadRNG@ for 'Eff' that delegates to
-- the @RNG@ effect:
--
-- >>> :set -XUndecidableInstances
--
-- >>> :{
--   instance RNG :> es => MonadRNG (Eff es) where
--     randomInt = send RandomInt
-- :}
--
-- Now we only need an interpreter:
--
-- >>> :{
--   runDummyRNG :: Eff (RNG : es) a -> Eff es a
--   runDummyRNG = interpret $ \_ -> \case
--     RandomInt -> pure 55
-- :}
--
-- and we can use any function that requires a @MonadRNG@ constraint with the
-- 'Eff' monad as long as the @RNG@ effect is in place:
--
-- >>> runEff . runDummyRNG $ randomString 3
-- "777"
--

----------------------------------------
-- Handling effects

-- | Interpret an effect.
interpret
  :: DispatchOf e ~ Dynamic
  => EffectHandler e es
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpret handler m = unsafeEff $ \es -> do
  (`unEff` es) $ runHandler (Handler es handler) m

-- | Interpret an effect using other, private effects.
--
-- @'interpret' ≡ 'reinterpret' 'id'@
reinterpret
  :: DispatchOf e ~ Dynamic
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> EffectHandler e handlerEs
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpret runHandlerEs handler m = unsafeEff $ \es -> do
  (`unEff` es) . runHandlerEs . unsafeEff $ \handlerEs -> do
    (`unEff` es) $ runHandler (Handler handlerEs handler) m

-- | Replace the handler of an existing effect with a new one.
--
-- /Note:/ this function allows for augmenting handlers with a new functionality
-- as the new handler can send operations to the old one.
--
-- >>> :{
--   data E :: Effect where
--     Op :: E m ()
--   type instance DispatchOf E = Dynamic
-- :}
--
-- >>> :{
--   runE :: IOE :> es => Eff (E : es) a -> Eff es a
--   runE = interpret $ \_ Op -> liftIO (putStrLn "op")
-- :}
--
-- >>> runEff . runE $ send Op
-- op
--
-- >>> :{
--   augmentE :: (E :> es, IOE :> es) => Eff es a -> Eff es a
--   augmentE = interpose $ \_ Op -> liftIO (putStrLn "augmented op") >> send Op
-- :}
--
-- >>> runEff . runE . augmentE $ send Op
-- augmented op
-- op
--
interpose
  :: forall e es a. (DispatchOf e ~ Dynamic, e :> es)
  => EffectHandler e es
  -- ^ The effect handler.
  -> Eff es a
  -> Eff es a
interpose handler m = unsafeEff $ \es -> do
  bracket (do
              origHandler <- getEnv @e es
              replaceEnv origHandler relinkHandler es
          )
          (\newEs -> do
              -- Restore the original handler.
              putEnv es =<< getEnv @e newEs
              unreplaceEnv @e newEs
          )
          (\newEs -> do
              -- Replace the original handler with a new one. Note that 'newEs'
              -- will still see the original handler.
              putEnv es (Handler newEs handler)
              unEff m es
          )

-- | Replace the handler of an existing effect with a new one that uses other,
-- private effects.
--
-- @'interpose' ≡ 'impose' 'id'@
impose
  :: forall e es handlerEs a b. (DispatchOf e ~ Dynamic, e :> es)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> EffectHandler e handlerEs
  -- ^ The effect handler.
  -> Eff es a
  -> Eff es b
impose runHandlerEs handler m = unsafeEff $ \es -> do
  bracket (do
              origHandler <- getEnv @e es
              replaceEnv origHandler relinkHandler es
          )
          (\newEs -> do
              -- Restore the original handler.
              putEnv es =<< getEnv @e newEs
              unreplaceEnv @e newEs
          )
          (\newEs -> do
              (`unEff` newEs) . runHandlerEs . unsafeEff $ \handlerEs -> do
                -- Replace the original handler with a new one. Note that
                -- 'newEs' (and thus 'handlerEs') wil still see the original
                -- handler.
                putEnv es (Handler handlerEs handler)
                unEff m es
          )

----------------------------------------
-- Unlifts

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnlift'.
localSeqUnlift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnlift (LocalEnv les) k = unsafeEff $ \es -> do
  seqUnliftIO les $ \unlift -> do
    (`unEff` es) $ k $ unsafeEff_ . unlift

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnliftIO'.
localSeqUnliftIO
  :: (HasCallStack, SharedSuffix es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnliftIO (LocalEnv les) k = liftIO $ seqUnliftIO les k

-- | Create a local unlifting function with the given strategy.
localUnlift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnlift (LocalEnv les) strategy k = case strategy of
  SeqUnlift -> unsafeEff $ \es -> do
    seqUnliftIO les $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
  ConcUnlift p l -> unsafeEff $ \es -> do
    concUnliftIO les p l $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift

-- | Create a local unlifting function with the given strategy.
localUnliftIO
  :: (HasCallStack, SharedSuffix es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnliftIO (LocalEnv les) strategy k = case strategy of
  SeqUnlift      -> liftIO $ seqUnliftIO les k
  ConcUnlift p l -> liftIO $ concUnliftIO les p l k

-- | Utility for lifting 'Eff' computations of type
--
-- @'Eff' es a -> 'Eff' es b@
--
-- to
--
-- @'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the computation must not run its argument in a different thread,
-- attempting to do so will result in a runtime error.
withLiftMap
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall a b. (Eff es a -> Eff es b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMap !_ k = unsafeEff $ \es -> do
  -- The LocalEnv parameter is not used, but we need it to constraint the
  -- localEs type variable. It's also strict so that callers don't cheat.
  (`unEff` es) $ k $ \mapEff m -> unsafeEff $ \localEs -> do
    seqUnliftIO localEs $ \unlift -> do
      (`unEff` es) . mapEff . unsafeEff_ $ unlift m

-- | Utility for lifting 'IO' computations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the computation must not run its argument in a different thread,
-- attempting to do so will result in a runtime error.
--
-- Useful e.g. for lifting the unmasking function in
-- 'Control.Exception.mask'-like computations:
--
-- >>> :{
-- data Fork :: Effect where
--   ForkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> Fork m ThreadId
-- type instance DispatchOf Fork = Dynamic
-- :}
--
-- >>> :{
-- runFork :: IOE :> es => Eff (Fork : es) a -> Eff es a
-- runFork = interpret $ \env (ForkWithUnmask m) -> withLiftMapIO env $ \liftMap -> do
--   localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
--     forkIOWithUnmask $ \unmask -> unlift $ m $ liftMap unmask
-- :}
withLiftMapIO
  :: (HasCallStack, SharedSuffix es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall a b. (IO a -> IO b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMapIO !_ k = k $ \mapIO m -> unsafeEff $ \es -> do
  -- The LocalEnv parameter is not used, but we need it to constraint the
  -- localEs type variable. It's also strict so that callers don't cheat.
  seqUnliftIO es $ \unlift -> mapIO $ unlift m

----------------------------------------
-- Bidirectional lifts

-- | Create a local lifting and unlifting function with the given strategy.
--
-- Useful for lifting complicated 'Eff' computations where the monadic action
-- shows in both positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the computation you're lifting 'localUnlift' along with
-- 'withLiftMap' might be enough and is more efficient.
localLiftUnlift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff es r -> Eff localEs r) -> (forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnlift (LocalEnv les) strategy k = case strategy of
  SeqUnlift -> unsafeEff $ \es -> do
    seqUnliftIO es $ \unliftEs -> do
      seqUnliftIO les $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (unsafeEff_ . unliftLocalEs)
  ConcUnlift p l -> unsafeEff $ \es -> do
    concUnliftIO es p l $ \unliftEs -> do
      concUnliftIO les p l $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (unsafeEff_ . unliftLocalEs)

-- | Create a local unlifting function with the given strategy along with an
-- unrestricted lifting function.
--
-- Useful for lifting complicated 'IO' computations where the monadic action
-- shows in both positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the computation you're lifting 'localUnliftIO' along
-- with 'withLiftMapIO' might be enough and is more efficient.
localLiftUnliftIO
  :: (HasCallStack, SharedSuffix es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. IO r -> Eff localEs r) -> (forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnliftIO (LocalEnv les) strategy k = case strategy of
  SeqUnlift      -> liftIO $ seqUnliftIO les $ k unsafeEff_
  ConcUnlift p l -> liftIO $ concUnliftIO les p l $ k unsafeEff_

----------------------------------------
-- Utils

-- | Require that both effect stacks share an opaque suffix.
--
-- Functions from the 'localUnlift' family utilize this constraint to guarantee
-- sensible usage of unlifting functions.
--
-- As an example, consider the following higher order effect:
--
-- >>> :{
--   data E :: Effect where
--     E :: m a -> E m a
--   type instance DispatchOf E = Dynamic
-- :}
--
-- Running local actions in a more specific environment is fine:
--
-- >>> :{
--  runE1 :: Eff (E ': es) a -> Eff es a
--  runE1 = interpret $ \env -> \case
--    E m -> runReader () $ do
--      localSeqUnlift env $ \unlift -> unlift m
-- :}
--
-- Running local actions in a more general environment is fine:
--
-- >>> :{
--  runE2 :: Eff (E ': es) a -> Eff es a
--  runE2 = reinterpret (runReader ()) $ \env -> \case
--    E m -> raise $ do
--      localSeqUnlift env $ \unlift -> unlift m
-- :}
--
-- However, running local actions in an unrelated environment is not fine as
-- this would make it possible to run anything within 'runPureEff':
--
-- >>> :{
--  runE3 :: Eff (E ': es) a -> Eff es a
--  runE3 = reinterpret (runReader ()) $ \env -> \case
--    E m -> pure . runPureEff $ do
--      localSeqUnlift env $ \unlift -> unlift m
-- :}
-- ...
-- ...Could not deduce (SharedSuffix '[] es)...
-- ...
--
-- Running local actions in a monomorphic effect stack is also not fine as
-- this makes a special case of the above possible:
--
-- >>> :{
--  runE4 :: Eff '[E, IOE] a -> Eff '[IOE] a
--  runE4 = interpret $ \env -> \case
--    E m -> pure . runPureEff $ do
--      localSeqUnlift env $ \unlift -> unlift m
-- :}
-- ...
-- ...Running local actions in monomorphic effect stacks is not supported...
-- ...
--
-- @since 1.2.0.0
class SharedSuffix (es1 :: [Effect]) (es2 :: [Effect])

instance {-# INCOHERENT #-} SharedSuffix es es
instance {-# INCOHERENT #-} SharedSuffix es1 es2 => SharedSuffix (e : es1) es2
instance {-# INCOHERENT #-} SharedSuffix es1 es2 => SharedSuffix es1 (e : es2)

-- | This is always preferred to @SharedSuffix es es@ as it's not incoherent.
instance
  TypeError
  ( Text "Running local actions in monomorphic effect stacks is not supported." :$$:
    Text "As a solution simply change the stack to have a polymorphic suffix."
  ) => SharedSuffix '[] '[]

-- $setup
-- >>> import Control.Concurrent (ThreadId, forkIOWithUnmask)
-- >>> import Effectful.Reader.Static
