module Data.Homogeneous.Variant
  ( homogeneous
  , Homogeneous
  , toVariant
  ) where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum, pred, succ, toEnum)
import Data.Homogeneous (class RowSList, class SListRow)
import Data.Maybe (Maybe)
import Data.Variant (class VariantBounded, class VariantBoundedEnums, class VariantEqs, class VariantOrds, class VariantShows, Variant)
import Data.Variant.Internal (class VariantTags, VariantRep(..))
import Prim.RowList (class RowToList)
import Record.Extra (kind SList)
import Type.Row.Homogeneous (class Homogeneous) as Row
import Unsafe.Coerce (unsafeCoerce)

newtype Homogeneous (sl ∷ SList) a
  = Homogeneous (VariantRep a)

toVariant ∷ ∀ a ra sl. SListRow sl a ra ⇒ Homogeneous sl a → Variant ra
toVariant (Homogeneous v) = unsafeCoerce v

homogeneous ∷ ∀ a ra sl. RowSList sl a ra ⇒ (Variant ra → Homogeneous sl a)
homogeneous = Homogeneous <<< unsafeCoerce

instance eqHomogeneous ∷ (SListRow sl a ra, Eq a, RowToList ra rl, VariantTags rl, VariantEqs rl) ⇒ Eq (Homogeneous sl a) where
  eq h1 h2 = eq (toVariant h1) (toVariant h2)

instance ordHomogeneous ∷ (SListRow sl a ra, Ord a, RowToList ra rl, VariantTags rl, VariantEqs rl, VariantOrds rl) ⇒ Ord (Homogeneous sl a) where
  compare h1 h2 = compare (toVariant h1 ∷ Variant ra) (toVariant h2 ∷ Variant ra)

instance enumHomogeneous ∷ (RowSList sl a ra, SListRow sl a ra, Ord a, RowToList ra rl, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBoundedEnums rl) ⇒ Enum (Homogeneous sl a) where
  pred h = homogeneous <$> pred (toVariant h ∷ Variant ra)
  succ h = homogeneous <$> succ (toVariant h ∷ Variant ra)

instance boundedHomogeneous ∷ (RowSList sl a ra, SListRow sl a ra, Ord a, RowToList ra rl, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBounded rl) ⇒ Bounded (Homogeneous sl a) where
  top = homogeneous (top ∷ Variant ra)
  bottom = homogeneous (bottom ∷ Variant ra)

instance boundedEnumHomogeneous ∷ (Row.Homogeneous ra a, RowSList sl a ra, SListRow sl a ra, Ord a, RowToList ra rl, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBoundedEnums rl) ⇒ BoundedEnum (Homogeneous sl a) where
  cardinality =
    let
      Cardinality c = cardinality ∷ Cardinality (Variant ra)
    in
      Cardinality c
  fromEnum h = fromEnum ((toVariant h) ∷ Variant ra)
  toEnum i = (homogeneous ∷ (Variant ra → Homogeneous sl a)) <$> (((toEnum i) ∷ Maybe (Variant ra))) ∷ Maybe (Homogeneous sl a)

instance functorHomogeneous ∷ Functor (Homogeneous r) where
  map f (Homogeneous (VariantRep r)) = Homogeneous (VariantRep r { value = f r.value })

instance extendHomogeneous ∷ Extend (Homogeneous r) where
  extend f h@(Homogeneous (VariantRep r)) = Homogeneous (VariantRep (r { value = f h }))

instance comonad ∷ Comonad (Homogeneous r) where
  extract (Homogeneous (VariantRep r)) = r.value

instance showHomogeneous ∷ (SListRow sl a ra, RowSList sl a ra, RowToList ra rl, Show a, VariantTags rl, VariantShows rl) ⇒ Show (Homogeneous sl a) where
  show v = "Homogeneous (" <> show (toVariant v ∷ Variant ra) <> ")"
