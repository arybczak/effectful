{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | Type-safe indexing for 'Effectful.Internal.Monad.Env'.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Effect
  ( Effect
  , (:>)(..)
  , (:>>)
  , Subset(..)
  , KnownSubset
  , KnownPrefix(..)
  , IsUnknownSuffixOf
  , type (++)
  , KnownEffects(..)

    -- * Unique parametrized effects
  , type (<:>)

    -- * Re-exports
  , Type
  ) where

import Data.Kind
import GHC.TypeLits

-- | The kind of effects.
type Effect = (Type -> Type) -> Type -> Type

-- | A constraint that requires that a particular effect @e@ is a member of the
-- type-level list @es@. This is used to parameterize an 'Effectful.Eff'
-- computation over an arbitrary list of effects, so long as @e@ is /somewhere/
-- in the list.
--
-- For example, a computation that only needs access to a mutable value of type
-- 'Integer' would have the following type:
--
-- @
-- 'Effectful.State.Static.Local.State' 'Integer' ':>' es => 'Effectful.Eff' es ()
-- @
class (e :: Effect) :> (es :: [Effect]) where
  -- | Get the position of @e@ in @es@.
  --
  -- /Note:/ GHC is kind enough to cache these values as they're top level CAFs,
  -- so the lookup is amortized @O(1)@ without any language level tricks.
  reifyIndex :: Int
  reifyIndex =
    -- Don't show "minimal complete definition" in haddock.
    error "reifyIndex"

instance TypeError
  ( Text "There is no handler for '" :<>: ShowType e :<>: Text "' in the context"
  ) => e :> '[] where
  reifyIndex = error "unreachable"

instance {-# OVERLAPPING #-} e :> (e : es) where
  reifyIndex = 0

instance e :> es => e :> (x : es) where
  reifyIndex = 1 + reifyIndex @e @es

----------------------------------------

-- | Convenience operator for expressing that a function uses multiple effects
-- in a more concise way than enumerating them all with '(:>)'.
--
-- @[E1, E2, ..., En] ':>>' es ≡ (E1 ':>' es, E2 ':>' es, ..., En :> es)@
type family xs :>> es :: Constraint where
  '[]      :>> es = ()
  (x : xs) :>> es = (x :> es, xs :>> es)
{-# DEPRECATED (:>>) "Usage of (:>>) slows down GHC too much. See https://github.com/haskell-effectful/effectful/issues/52#issuecomment-1269155485 for more information." #-}

----------------------------------------

-- | Provide evidence that @subEs@ is a subset of @es@.
class KnownPrefix es => Subset (subEs :: [Effect]) (es :: [Effect]) where
  subsetFullyKnown :: Bool
  subsetFullyKnown =
    -- Don't show "minimal complete definition" in haddock.
    error "subsetFullyKnown"

  reifyIndices :: [Int]
  reifyIndices =
    -- Don't show "minimal complete definition" in haddock.
    error "reifyIndices"

-- If the subset is not fully known, make sure the subset and the base stack
-- have the same unknown suffix.
instance {-# INCOHERENT #-}
  ( KnownPrefix es
  , subEs `IsUnknownSuffixOf` es
  ) => Subset subEs es where
  subsetFullyKnown = False
  reifyIndices = []

-- If the subset is fully known, we're done.
instance KnownPrefix es => Subset '[] es where
  subsetFullyKnown = True
  reifyIndices = []

instance (e :> es, Subset subEs es) => Subset (e : subEs) es where
  subsetFullyKnown = subsetFullyKnown @subEs @es
  reifyIndices = reifyIndex @e @es : reifyIndices @subEs @es

----

-- | Provide evidence that @subEs@ is a known subset of @es@.
class Subset subEs es => KnownSubset (subEs :: [Effect]) (es :: [Effect])
instance KnownSubset '[] es
instance (e :> es, KnownSubset subEs es) => KnownSubset (e : subEs) es

----

-- | Calculate length of a statically known prefix of @es@.
class KnownPrefix (es :: [Effect]) where
  prefixLength :: Int

instance KnownPrefix es => KnownPrefix (e : es) where
  prefixLength = 1 + prefixLength @es

instance {-# INCOHERENT #-} KnownPrefix es where
  prefixLength = 0

----

-- | Require that @subEs@ is the unknown suffix of @es@.
class (subEs :: [Effect]) `IsUnknownSuffixOf` (es :: [Effect])
instance {-# INCOHERENT #-} subEs ~ es => subEs `IsUnknownSuffixOf` es
instance subEs `IsUnknownSuffixOf` es => subEs `IsUnknownSuffixOf` (e : es)

----

-- | Append two type-level lists together.
type family (xs :: [Effect]) ++ (ys :: [Effect]) :: [Effect] where
  '[] ++ ys  = ys
  (x : xs) ++ ys = x : xs ++ ys

infixr 5 ++

-- | Calculate length of a list of known effects.
class KnownEffects (es :: [Effect]) where
  knownEffectsLength :: Int
  knownEffectsLength =
  -- Don't show "minimal complete definition" in haddock.
    error "knownEffectsLength"

instance KnownEffects es => KnownEffects (e : es) where
  knownEffectsLength = 1 + knownEffectsLength @es

instance KnownEffects '[] where
  knownEffectsLength = 0

----------------------------------------

type family (e :: Effect) <:> (es :: [Effect]) :: Constraint where
  e a1 a2 a3 a4 a5 <:> es = Has5UniqueParams e a1 a2 a3 a4 a5 es
  e a1 a2 a3 a4 <:> es = Has4UniqueParams e a1 a2 a3 a4 es
  e a1 a2 a3 <:> es = Has3UniqueParams e a1 a2 a3 es
  e a1 a2 <:> es = Has2UniqueParams e a1 a2 es
  e a1 <:> es = HasUniqueParam e a1 es

class e a1 :> es => HasUniqueParam
  (e :: k1 -> Effect)
  (a1 :: k1)
  (es :: [Effect])
  | e es -> a1

class e a1 a2 :> es => Has2UniqueParams
  (e :: k1 -> k2 -> Effect)
  (a1 :: k1)
  (a2 :: k2)
  (es :: [Effect])
  | e es -> a1 a2

class e a1 a2 a3 :> es => Has3UniqueParams
  (e :: k1 -> k2 -> k3 -> Effect)
  (a1 :: k1)
  (a2 :: k2)
  (a3 :: k3)
  (es :: [Effect])
  | e es -> a1 a2 a3

class e a1 a2 a3 a4 :> es => Has4UniqueParams
  (e :: k1 -> k2 -> k3 -> k4 -> Effect)
  (a1 :: k1)
  (a2 :: k2)
  (a3 :: k3)
  (a4 :: k4)
  (es :: [Effect])
  | e es -> a1 a2 a3 a4

class e a1 a2 a3 a4 a5 :> es => Has5UniqueParams
  (e :: k1 -> k2 -> k3 -> k4 -> k5 -> Effect)
  (a1 :: k1)
  (a2 :: k2)
  (a3 :: k3)
  (a4 :: k4)
  (a5 :: k5)
  (es :: [Effect])
  | e es -> a1 a2 a3 a4 a5
