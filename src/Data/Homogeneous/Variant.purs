module Data.Homogeneous.Variant where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Homogeneous.Record (ConstRow)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Homogeneous (row ∷ # Type) a = Homogeneous (VariantRep a)

instance functorHomogeneous ∷ Functor (Homogeneous r) where
  map f (Homogeneous (VariantRep r)) = Homogeneous (VariantRep r{ value = f r.value })

instance extendHomogeneous ∷ Extend (Homogeneous r) where
  extend f h@(Homogeneous (VariantRep r)) =
    let
      b = f h
    in
      Homogeneous (VariantRep (r{ value = b}))

instance comonad ∷ Comonad (Homogeneous r) where
  extract (Homogeneous (VariantRep r)) = r.value

homogeneous ∷ ∀ a r r'. ConstRow Unit r r' (Variant r → Homogeneous r' a)
homogeneous = Homogeneous <<< unsafeCoerce

toVariant ∷ ∀ a r r'. ConstRow a r r' (Homogeneous r a → Variant r')
toVariant (Homogeneous v) = unsafeCoerce v
