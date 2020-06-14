module Data.Homogeneous.Variant
  ( homogeneous
  , Homogeneous
  , toVariant
  )
  where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Homogeneous (class Fill)
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Homogeneous (row ∷ # Type) a = Homogeneous (VariantRep a)

instance functorHomogeneous ∷ Functor (Homogeneous r) where
  map f (Homogeneous (VariantRep r)) = Homogeneous (VariantRep r{ value = f r.value })

instance extendHomogeneous ∷ Extend (Homogeneous r) where
  extend f h@(Homogeneous (VariantRep r)) = Homogeneous (VariantRep (r{ value = f h}))

instance comonad ∷ Comonad (Homogeneous r) where
  extract (Homogeneous (VariantRep r)) = r.value

homogeneous ∷ ∀ a ra rv. Fill Void ra rv ⇒ (Variant ra → Homogeneous rv a)
homogeneous = Homogeneous <<< unsafeCoerce

toVariant ∷ ∀ a ra rv. Fill a ra rv ⇒ Homogeneous rv a → Variant ra
toVariant (Homogeneous v) = unsafeCoerce v

