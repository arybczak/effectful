# effectful

[![Build Status](https://github.com/haskell-effectful/effectful/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/haskell-effectful/effectful/actions?query=branch%3Amaster)
[![Hackage](https://img.shields.io/hackage/v/effectful.svg)](https://hackage.haskell.org/package/effectful)
[![Dependencies](https://img.shields.io/hackage-deps/v/effectful.svg)](https://packdeps.haskellers.com/feed?needle=andrzej@rybczak.net)
[![Stackage LTS](https://www.stackage.org/package/effectful/badge/lts)](https://www.stackage.org/lts/package/effectful)
[![Stackage Nightly](https://www.stackage.org/package/effectful/badge/nightly)](https://www.stackage.org/nightly/package/effectful)


<img src="https://user-images.githubusercontent.com/387658/127747903-f728437f-2ee4-47b8-9f0c-5102fd44c8e4.png" width="128">

An easy to use, fast extensible effects library with seamless integration with
the existing Haskell ecosystem.

Main features:

1. Very fast
   ([benchmarks](https://github.com/haskell-effectful/effectful/tree/master/benchmarks/README.md)).

2. Easy to use API (comparable with usage of the [MonadUnliftIO](https://hackage.haskell.org/package/unliftio-core/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO) class).

3. Correct semantics in presence of runtime exceptions (no more discarded state
   updates).

4. Seamless integration with the existing ecosystem (`exceptions`,
   `monad-control`, `unliftio-core`, `resourcet` etc.).

5. Support for thread local and shared state (e.g. `StateT` provides a thread
   local state, while `MVar` holds a shared state, both approaches have their
   merits).

6. Support for statically (implementation determined at compile time) and
   dynamically (implementation determined at run time) dispatched effects.

## Motivation

Do we really need yet another library for handling effects? There's
[freer-simple](https://hackage.haskell.org/package/freer-simple),
[fused-effects](https://hackage.haskell.org/package/fused-effects),
[polysemy](https://hackage.haskell.org/package/polysemy),
[eff](https://github.com/hasura/eff) and probably a few more.

Unfortunately, of all of them only `eff` is a promising proposition because of
reasonable performance characteristics (see the talk [Effects for
Less](https://www.youtube.com/watch?v=0jI-AlWEwYI) for more information) and
potential for good interoperability with the existing ecosystem.

The second point is arguably the most important, because it allows focusing on
things that matter instead of reinventing all kinds of wheels, hence being a
necessary condition for broader adoption of the library.

However, `eff` uses delimited continuations underneath, which:

- Are not yet supported by GHC (though [the
proposal](https://github.com/ghc-proposals/ghc-proposals/pull/313) for including
support for them has been accepted).

- Are quite hard to understand.

- Make the library "too powerful" in a sense as it faces
  [a](https://github.com/hasura/eff/issues/13)
  [few](https://github.com/hasura/eff/issues/7)
  [issues](https://github.com/hasura/eff/issues/12) with no clear path towards
  their resolution.

### What about `mtl`?

It's true that its "effects as classes" approach is widely known and used often.

However:

- `mtl` style effects are
  [slow](https://github.com/haskell-effectful/effectful/tree/master/benchmarks/README.md).

- The majority of popular monad transformers (except `ReaderT`) used for effect
  implementations are rife with [subtle
  issues](https://github.com/haskell-effectful/effectful/tree/master/transformers.md).

These are problematic enough that the [ReaderT design
pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) was
invented. Its fundamentals are solid, but it's not an effect system.

A solution? Use the `ReaderT` pattern as a base and build around it to make an
extensible effects library! This is where `effectful` comes in. The `Eff` monad
it uses is essentially a `ReaderT` over `IO` on steroids, allowing us to extend
its environment with data types representing effects.

This concept is quite simple, so:

- It's reasonably easy to understand what is going on under the hood.

- The `Eff` monad being a reader allows for seamless interoperability with
  ubiquitous classes such as `MonadBaseControl` and `MonadUnliftIO` and solves
  [issues](https://github.com/haskell-effectful/effectful/tree/master/transformers.md)
  of monad transformers mentioned above.

What is more, the `Eff` monad is concrete, so GHC has many possibilities for
optimization, which results in a very fast code at a default optimization
level. There is no need to mark every function `INLINE` or enable additional
optimization passes, it just works.

### Any downsides?

As always, there's no free lunch. The `Eff` monad doesn't support `NonDet` nor
`Coroutine` effects. However, the `NonDet` effect in existing libraries is
[broken](https://github.com/lexi-lambda/eff/blob/8c4df4bf54faf22456354be18095b14825be5e85/notes/semantics-zoo.md)
and none of the ones with support for higher order effects provide the
`Coroutine` effect, so arguably it's not a big loss.

If you need such capability in your application, there are well established
libraries such as [conduit](https://hackage.haskell.org/package/conduit) or
[list-t](https://hackage.haskell.org/package/list-t) that can be used with
`effectful` without any issues.

### Summary

`effectful` is an extensible effects library that aims to replace "boring"
transformer stacks (which consist of a dozen of newtype'd `ExceptT`, `ReaderT`,
`StateT` and `WriterT` transformers) and their derivatives by providing
equivalent effects with improved semantics, performance and usability (it also
makes it easy to reuse them for your own effects). It doesn't try to make monad
transformers obsolete, so you're free to use it with `ConduitT`, `ContT`,
`ListT` etc. when necessary.

## Package structure

The effect system is split among several libraries:

- The [`effectful-core`](https://hackage.haskell.org/package/effectful-core)
  library contains the core of the effect system along with the basic
  effects. It aims for a small dependency footprint and provides building blocks
  for more advanced effects.

- The [`effectful-plugin`](https://hackage.haskell.org/package/effectful-plugin)
  library provides an optional GHC plugin for improving disambiguation of
  effects (see
  [here](https://github.com/haskell-effectful/effectful/blob/master/effectful-plugin/README.md)
  for more information).

- The [`effectful-th`](https://hackage.haskell.org/package/effectful-th) library
  provides utilities for generating bits of effect-related boilerplate via
  Template Haskell.

- The [`effectful`](https://hackage.haskell.org/package/effectful) library
  re-exports public modules of `effectful-core` and additionally provides most
  features of the `unliftio` library divided into appropriate effects.

## Example

For the examples see the *Introduction* sections of
[`Effectful.Dispatch.Dynamic`](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Dynamic.html)
and
[`Effectful.Dispatch.Static`](https://hackage.haskell.org/package/effectful-core/docs/Effectful-Dispatch-Static.html).

## Resources

Resources that inspired the rise of this library and had a lot of impact on its
design.

Talks:

* [Effects for Less](https://www.youtube.com/watch?v=0jI-AlWEwYI) by Alexis King.

* [Monad Transformer State](https://www.youtube.com/watch?v=KZIN9f9rI34) by Michael Snoyman.

Blog posts:

* [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) by Michael Snoyman.

* [Exceptions Best Practices](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/) by Michael Snoyman.

----------------------------------------

<div>Icons made by <a href="https://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a></div>
