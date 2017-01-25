module Halogen.StartApp.Internal.AltF
  ( AltF(..)
  , Fused
  , toArray
  , transform
  ) where

import Prelude

import Data.Array as Array
import Data.Monoid (class Monoid)

import Unsafe.Coerce (unsafeCoerce)

data AltF f a
  = Single (f a)
  | Batch (Array (AltF f a))
  | Fuse (Fused f a)

data FusedF g b f a = Fused (f ~> g) (a -> b) (AltF f a)

data Fused (f :: * -> *) a

mkFused :: forall f a g b. (f ~> g) -> (a -> b) -> AltF f a -> Fused g b
mkFused nat f a = unsafeCoerce (Fused nat f a)

runFused :: forall g b. Functor g => Fused g b -> AltF g b
runFused a =
  case unsafeCoerce a of
    Fused nat f b -> go nat f b
  where
    go :: forall f a. (f ~> g) -> (a -> b) -> AltF f a -> AltF g b
    go nat f = case _ of
      Single c -> Single (f <$> nat c)
      Batch cs -> Batch (go nat f <$> cs)
      Fuse c   -> runFused (transformFused nat f c)

transformFused :: forall f a g b. (f ~> g) -> (a -> b) -> Fused f a -> Fused g b
transformFused nat' g a =
  case unsafeCoerce a of
    Fused nat f b -> mkFused (nat' <<< nat) (g <<< f) b

toArray :: forall f a. Functor f => AltF f a -> Array (f a)
toArray = case _ of
  Single a -> [ a ]
  Batch as -> toArray =<< as
  Fuse a   -> toArray (runFused a)

transform :: forall f a g b. (f ~> g) -> (a -> b) -> AltF f a -> AltF g b
transform nat f = case _ of
  Fuse a -> Fuse (transformFused nat f a)
  a      -> Fuse (mkFused nat f a)

instance functorFused :: Functor (Fused f) where
  map f = transformFused id f

instance semigroupAltF :: Semigroup (AltF f a) where
  append (Batch as) (Batch bs) = Batch (as <> bs)
  append (Batch as) b = Batch (Array.snoc as b)
  append a (Batch bs) = Batch (Array.cons a bs)
  append a b = Batch [a, b]

instance monoidAltF :: Monoid (AltF f a) where
  mempty = Batch []

instance functorAltF :: Functor (AltF f) where
  map f (Fuse a) = Fuse (f <$> a)
  map f a        = Fuse (mkFused id f a)
