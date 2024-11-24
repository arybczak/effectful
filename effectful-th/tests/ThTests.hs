{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Kind (Type)
import Data.String
import GHC.TypeLits

import Effectful
import Effectful.TH

main :: IO ()
main = pure () -- only compilation tests

data Test1 a :: Effect where
  Test1 :: Test1 a m a
makeUniqueEffect ''Test1

data Test2 a b :: Effect where
  Test2 :: Test2 a b m (a, b)
  AmbTest :: Test2 a b m ()
makeUniqueEffect ''Test2

test :: (Num n, IsString s, Test1 n <:> es, Test2 s Char <:> es) => Eff es ()
test = do
  _ <- test1
  _ <- test2
  ambTest

data SimpleADT (m :: Type -> Type) (a :: Type)
  = SimpleADTC1 Int
  | SimpleADTC2 String
  -- ^ This one does the second thing

-- Test generation of fixity information.
infixl 1 `SimpleADTC1`

makeEffect ''SimpleADT

data ADTSyntax1 (m :: Type -> Type) (a :: Type)
  = a ~ Int => ADTSyntax1C String

makeEffect ''ADTSyntax1

data ADTSyntax2 (m :: Type -> Type) (a :: Type)
  = a ~ Int    => ADTSyntax2C1 Int
  | a ~ String => ADTSyntax2C2 String

makeEffect ''ADTSyntax2

data ADTSyntax3 (m :: Type -> Type) (a :: Type)
  = Show a => ADTSyntax3C a

makeEffect ''ADTSyntax3

data GADTSyntax :: Effect where
  GADTSyntaxC1 :: Int -> GADTSyntax m Int
  -- | I am documented
  GADTSyntaxC2 :: String -> GADTSyntax m String
  GADTSyntaxC3 :: IOE :> es => Bool -> GADTSyntax (Eff es) a

makeEffect ''GADTSyntax

data Fields (m :: Type -> Type) (a :: Type)
  = FieldsC { fieldsCF1 :: Int, fieldsCF2 :: String }

makeEffect ''Fields

newtype Newtype1 (m :: Type -> Type) (a :: Type)
  = Newtype1C Int

makeEffect ''Newtype1

newtype Newtype2 :: Effect where
  Newtype2C :: String -> Newtype2 m a

makeEffect ''Newtype2

data Instance = ADTI | GADTI | NTI | MMI

data family Family (s :: Instance) (m :: Type -> Type) a

data instance Family 'ADTI _ _ = ADTIC1 Int | ADTIC2 String
makeEffect 'ADTIC1

data instance Family 'GADTI _ _ where
  GADTIC1 :: Int -> Family 'GADTI m Int
  GADTIC2 :: String -> Family 'GADTI m String
makeEffect 'GADTIC1

newtype instance Family 'NTI _ _ = NTIC Int
makeEffect 'NTIC

data instance Family 'MMI m (_ m) where
  MMIC1 :: f m -> Family 'MMI m (f m)
  MMIC2 :: (forall x. m x -> m (f m)) -> Family 'MMI m (f m)

makeEffect 'MMIC1

data Complex :: Effect where
  Mono            :: Int -> Complex m Bool
  Poly            :: a -> Complex m a
  PolyIn          :: a -> Complex m Bool
  PolyOut         :: Int -> Complex m a
  Lots            :: a -> b -> c -> d -> e -> f -> Complex m ()
  Nested          :: Maybe b -> Complex m (Maybe a)
  MultiNested     :: (Maybe a, [b]) -> Complex m (Maybe a, [b])
  Existential     :: (forall e. e -> Maybe e) -> Complex m a
  LotsNested      :: Maybe a -> [b] -> (c, c) -> Complex m (a, b, c)
  Dict            :: Ord a => a -> Complex m a
  MultiDict       :: (Eq a, Ord b, Enum a, Num c)
                  => a -> b -> c -> Complex m ()
  IndexedMono     :: f 0 -> Complex m Int
  IndexedPoly     :: forall f (n :: Nat) m . f n -> Complex m (f (n + 1))
  IndexedPolyDict :: KnownNat n => f n -> Complex m Int

makeEffect ''Complex

data HOEff :: Effect where
  EffArgMono :: m () -> HOEff m ()
  EffArgPoly :: m a -> HOEff m a
  EffArgComb :: m a -> (m a -> m b) -> HOEff m b
  EffRank2   :: (forall x. m x -> m (Maybe x)) -> HOEff m a

makeEffect ''HOEff

data ComplexEffArgs b c :: Effect where
  EffMono     :: Int -> ComplexEffArgs Int String m Bool
  EffPoly1    :: a -> ComplexEffArgs a b m a
  EffPoly2    :: a -> ComplexEffArgs a (Maybe a) m Bool
  EffPolyFree :: String -> ComplexEffArgs a b m Int
  EffSame1    :: ComplexEffArgs a a m a
  EffSame2    :: ComplexEffArgs b b m a
  EffHO       :: m b -> ComplexEffArgs b Int m String

makeEffect ''ComplexEffArgs

data HKEffArgs f g :: Effect where
  HKRank2 :: (forall x . f x -> g x) -> HKEffArgs f g m a

makeEffect ''HKEffArgs

data ByCon :: Effect where
  ByConA :: Int -> ByCon m String
  ByConB :: Int -> ByCon m String

makeEffect 'ByConA

data ByField :: Effect where
  ByFieldA :: { byFieldAf :: Int } -> ByField m Int
  ByFieldB :: { byFieldBf :: Int } -> ByField m Int

makeEffect 'byFieldAf

type family F ty
data AmbEff :: Effect where
  AmbEff :: Int -> AmbEff m (F ty)

-- This only works in GHC >= 9, otherwise the 'ty' variable is ambiguous.
#if __GLASGOW_HASKELL__ >= 900
makeEffect 'AmbEff
#endif
