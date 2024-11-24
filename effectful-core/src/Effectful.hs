module Effectful
  ( -- * Introduction
    -- $intro

    -- ** Integration with existing libraries
    -- $integration

    -- *** Transformed monads
    -- $transformer

    -- *** Concrete monads

    -- **** IO
    -- $concrete_io

    -- **** Other
    -- $concrete_other

    -- *** Polymorphic monads
    -- $poly

    -- * The 'Eff' monad
    Eff

    -- ** Effect constraints
  , Effect
  , Dispatch(..)
  , DispatchOf
  , (:>)
  , type (<:>)
  , (:>>)

    -- * Running the 'Eff' monad

    -- ** Pure computations
  , runPureEff

    -- ** Computations with side effects
  , runEff
  , IOE

    -- ** Unlifting
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withSeqEffToIO
  , withEffToIO
  , withConcEffToIO

    -- ** Lifting
  , raise
  , raiseWith
  , subsume
  , inject
  , Subset

    -- * Re-exports
  , MonadIO(..)
  , MonadUnliftIO(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- $intro
--
-- Haskell is one of the few programming languages that distinguishes between
-- pure functions and functions that might perform side effects. For example, a
-- function
--
-- @
-- f :: 'Int' -> 'String'
-- @
--
-- can't perform side effects at all, but a function
--
-- @
-- f :: 'Int' -> 'IO' 'String'
-- @
--
-- can perform any side effect. This "all or nothing" approach isn't very
-- satisfactory though, because the vast majority of time we would like to
-- signify that a function can perform /some/ side effects, e.g. only be able to
-- log messages.
--
-- This library provides support for expressing exactly that with its 'Eff'
-- monad:
--
-- @
-- f :: Log ':>' es => 'Int' -> 'Eff' es 'String'
-- @
--
-- It implements support for extensible effects with both dynamic and static
-- dispatch. For more information about each type consult the documentation in
-- "Effectful.Dispatch.Dynamic" and "Effectful.Dispatch.Static" (when in doubt,
-- start with dynamic dispatch).
--
-- The library provides:
--
-- - The 'Eff' monad that tracks effects at the type level. This is going to be
--   the main monad of your application.
--
-- - A set of predefined, basic effects such as t'Effectful.Error.Static.Error',
--   t'Effectful.Reader.Static.Reader', t'Effectful.State.Static.Local.State' and
--   t'Effectful.Writer.Static.Local.Writer'.
--
-- - Utilities for defining new effects and interpreting them, possibly in terms
--   of already existing ones (see introduction of the
--   "Effectful.Dispatch.Dynamic" module for more information).
--
-- While basic effects can be used out of the box, in general it's recommended
-- to create your own that serve a more specific purpose.
--

-- $integration
--
-- Integration with most of existing libraries and frameworks can be done quite
-- easily. The main difference in how that looks like depends on the way a
-- library operates in a monadic context.
--
-- There are three main groups a library might fall into. It either operates:
--
-- 1) In a monad of your application transformed by a library specific monad
--    transformer.
--
-- 2) In its own, concrete monad, which is usually 'IO' or a couple of monad
--    transformers on top of 'IO'.
--
-- 3) In a polymorphic monad, which is constrained by a type class that
--    implements core operations of a library.
--
-- Each case needs a slightly different approach to integrate with the 'Eff'
-- monad.

-- $transformer
--
-- These are libraries that provide a custom transformer for the main monad of
-- your application and their operations make use of it for their
-- operations. Examples include @InputT@ from the
-- [haskeline](https://hackage.haskell.org/package/haskeline) package or
-- @ConduitT@ from the [conduit](https://hackage.haskell.org/package/conduit)
-- package.
--
-- These libraries can trivially be used with the 'Eff' monad since it provides
-- typical instances that these libraries require the underlying monad to have,
-- such as t'Effectful.Exception.MonadMask' or 'MonadUnliftIO'.
--
-- In case the 'Eff' monad doesn't provide a specific instance out of the box,
-- it can be supplied via an effect. As an example see how the instance of
-- @MonadResource@ for 'Eff' is implemented in the
-- [resourcet-effectful](https://hackage.haskell.org/package/resourcet-effectful)
-- package.
--

-- $concrete_io
--
-- If a library operates in 'IO', there are a couple of ways to integrate it.
--
-- The easiest way is to use its functions selectively in the 'Eff' monad with
-- the help of 'liftIO' or 'withEffToIO'. However, this is not particularly
-- robust, since it vastly broadens the scope in which the 'IOE' effect is
-- needed (not to mention that explicit lifting is annoying).
--
-- A somewhat better approach is to create a dummy static effect with
-- lightweight wrappers of the library functions. As an example have a look at
-- the
-- [@Effectful.Concurrent.Async@](https://hackage.haskell.org/package/effectful/docs/Effectful-Concurrent-Async.html)
-- module from the [effectful](https://hackage.haskell.org/package/effectful)
-- package that wraps the API of the
-- [async](https://hackage.haskell.org/package/async) package. Unfortunately,
-- this requires the amount of work proportional to the size of the library and
-- might not be the best option, especially if you only need to make use of a
-- tiny portion of the API.
--
-- Even better (though sometimes hard to do in practice) way is to consider,
-- what do you need the library for and then create a custom effect with high
-- level operations that the library in question will help us implement. The
-- advantage of this approach is that we're hiding implementation details from
-- the so-called "business logic" of our application and make it possible to
-- easily swap them in different environments or during future refactoring.
--

-- $concrete_other
--
-- Some libraries operate in a transformer stack over 'IO' or have its own
-- concrete monad that's a newtype over 'IO', e.g. @Handler@ from the
-- [servant-server](https://hackage.haskell.org/package/servant-server) package.
--
-- In such case it's best to mirror the monad in question by the 'Eff' monad
-- with appropriate effects (as most popular monad transformers have [subtle
-- issues](https://github.com/haskell-effectful/effectful/blob/master/transformers.md)),
-- use it as soon as possible, then at the end feed the final state to the monad
-- of the library so it proceeds as if nothing unusual happened.
--
-- As an example, consider the following monad:
--
-- >>> import Control.Monad.State qualified as T
-- >>> import Control.Monad.Except qualified as T
--
-- >>> data HandlerState
-- >>> data HandlerError
--
-- >>> :{
--   newtype Handler a = Handler (T.ExceptT HandlerError (T.StateT HandlerState IO) a)
--     deriving ( Applicative, Functor, Monad, MonadIO
--              , T.MonadState HandlerState, T.MonadError HandlerError
--              )
-- :}
--
-- This is how you can execute 'Eff' actions in the @Handler@ monad:
--
-- >>> import Effectful.Error.Static
-- >>> import Effectful.State.Static.Local
--
-- >>> :{
--   effToHandler :: Eff [Error HandlerError, State HandlerState, IOE] a -> Handler a
--   effToHandler m = do
--     -- Retrieve the current state of the Handler.
--     s <- T.get
--     -- Run the Eff monad with effects mirroring the capabilities of @Handler@.
--     (er, s') <- liftIO . runEff . runState s . runErrorNoCallStack @HandlerError $ m
--     -- Update the state of the Handler and throw an error if appropriate.
--     T.put s'
--     either T.throwError pure er
-- :}
--

-- $poly
--
-- Libraries working in a polymorphic monad use @mtl@ style effects. Details
-- about their integration with the 'Eff' monad require familiarity with
-- dynamically dispatched effects and thus are available in the
-- "Effectful.Dispatch.Dynamic#integration" module.
--
