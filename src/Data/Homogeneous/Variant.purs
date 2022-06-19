module Data.Homogeneous.Variant
  ( homogeneous
  , homogeneous'
  , Homogeneous
  , fromHomogeneous
  ) where

import Prelude
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum, pred, succ, toEnum)
import Data.Foldable (class Foldable, foldMapDefaultL, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldrWithIndexDefault)
import Data.Homogeneous (class ListToHomogeneous, class HomogeneousRowLabels, class ToHomogeneousRow)
import Data.Maybe (Maybe)
import Data.Semigroup.Foldable (class Foldable1, foldMap1DefaultL, foldr1Default)
import Data.Variant (class VariantBounded, class VariantBoundedEnums, class VariantEqs, class VariantOrds, class VariantShows, Variant)
import Data.Variant.Internal (class VariantTags, VariantRep(..))
import Prim.RowList (class RowToList)
import Type.Row.Homogeneous (class HomogeneousRowList) as Row
import Unsafe.Coerce (unsafeCoerce)

newtype Homogeneous (ls :: Row Type) a = Homogeneous (VariantRep a)

fromHomogeneous :: forall a ra ls. ToHomogeneousRow ls a ra => Homogeneous ls a -> Variant ra
fromHomogeneous (Homogeneous v) = unsafeCoerce v

homogeneous :: forall a ra ls. HomogeneousRowLabels ra a ls => Variant ra -> Homogeneous ls a
homogeneous = Homogeneous <<< unsafeCoerce

-- | When you have labels `Row` and `a` at hand and want to derive the `Row` itself
-- | you can use this constructor.
homogeneous' :: forall a ra ls. ToHomogeneousRow ls a ra => Variant ra -> Homogeneous ls a
homogeneous' = Homogeneous <<< unsafeCoerce

instance (ToHomogeneousRow ls a ra, Eq a, RowToList ra rl, VariantTags rl, VariantEqs rl) => Eq (Homogeneous ls a) where
  eq h1 h2 = eq (fromHomogeneous h1) (fromHomogeneous h2)

instance (ToHomogeneousRow ls a ra, Ord a, RowToList ra rl, VariantTags rl, VariantEqs rl, VariantOrds rl) => Ord (Homogeneous ls a) where
  compare h1 h2 = compare (fromHomogeneous h1 :: Variant ra) (fromHomogeneous h2 :: Variant ra)

instance (HomogeneousRowLabels ra a ls, ToHomogeneousRow ls a ra, Ord a, RowToList ra rl, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBoundedEnums rl) => Enum (Homogeneous ls a) where
  pred h = homogeneous <$> pred (fromHomogeneous h :: Variant ra)
  succ h = homogeneous <$> succ (fromHomogeneous h :: Variant ra)

instance (HomogeneousRowLabels ra a ls, ToHomogeneousRow ls a ra, Ord a, RowToList ra rl, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBounded rl) => Bounded (Homogeneous ls a) where
  top = homogeneous (top :: Variant ra)
  bottom = homogeneous (bottom :: Variant ra)

instance (ListToHomogeneous rl Void ls, HomogeneousRowLabels ra a ls, ToHomogeneousRow ls a ra, Ord a, RowToList ra rl, Row.HomogeneousRowList rl a, VariantTags rl, VariantEqs rl, VariantOrds rl, VariantBoundedEnums rl) => BoundedEnum (Homogeneous ls a) where
  cardinality =
    let
      Cardinality c = cardinality :: Cardinality (Variant ra)
    in
      Cardinality c
  fromEnum h = fromEnum ((fromHomogeneous h) :: Variant ra)
  toEnum i = (homogeneous :: (Variant ra -> Homogeneous ls a)) <$> (((toEnum i) :: Maybe (Variant ra))) :: Maybe (Homogeneous ls a)

instance Functor (Homogeneous r) where
  map f (Homogeneous (VariantRep r)) = Homogeneous (VariantRep r { value = f r.value })

instance Extend (Homogeneous r) where
  extend f h@(Homogeneous (VariantRep r)) = Homogeneous (VariantRep (r { value = f h }))

instance Comonad (Homogeneous r) where
  extract (Homogeneous (VariantRep r)) = r.value

instance Foldable (Homogeneous r) where
  foldl f z (Homogeneous (VariantRep { value })) = f z value
  foldr f = foldrDefault f
  foldMap f = foldMapDefaultL f

instance FoldableWithIndex String (Homogeneous r) where
  foldlWithIndex f z (Homogeneous (VariantRep { "type": t, value })) = f t z value
  foldrWithIndex f = foldrWithIndexDefault f
  foldMapWithIndex f = foldMapWithIndexDefaultL f

instance Foldable1 (Homogeneous r) where
  foldl1 _ (Homogeneous (VariantRep { value })) = value
  foldr1 v = foldr1Default v
  foldMap1 f = foldMap1DefaultL f

instance (ToHomogeneousRow ls a ra, HomogeneousRowLabels ls a ra, RowToList ra rl, Show a, VariantTags rl, VariantShows rl) => Show (Homogeneous ls a) where
  show v = "Homogeneous (" <> show (fromHomogeneous v :: Variant ra) <> ")"
