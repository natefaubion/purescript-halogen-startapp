module Halogen.StartApp.Cmd
  ( Cmd
  , run
  , perform
  , batch
  , hoist
  ) where

import Prelude
import Data.Monoid (class Monoid, mempty)
import Unsafe.Coerce (unsafeCoerce)

import Halogen.StartApp.Internal.AltF (AltF)
import Halogen.StartApp.Internal.AltF as AltF

newtype Cmd f a = Cmd (AltF f a)

run :: forall f a. Functor f => Cmd f a -> Array (f a)
run (Cmd f) = AltF.toArray f

perform :: forall f a. f a -> Cmd f a
perform = Cmd <<< AltF.Single

batch :: forall f a. Array (Cmd f a) -> Cmd f a
batch = Cmd <<< AltF.Batch <<< unsafeCoerce

hoist :: forall f g a. (f ~> g) -> Cmd f a -> Cmd g a
hoist f (Cmd a) = Cmd (AltF.transform f id a)

instance semigroupCmd :: Semigroup (Cmd f a) where
  append (Cmd a) (Cmd b) = Cmd (a <> b)

instance monoidCmd :: Monoid (Cmd f a) where
  mempty = Cmd mempty

instance functorCmd :: Functor (Cmd f) where
  map f (Cmd a) = Cmd (f <$> a)
