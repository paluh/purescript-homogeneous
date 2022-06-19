module Data.Homogeneous.Record
  ( homogeneous
  , homogeneous'
  , Homogeneous
  , fromHomogeneous
  , modify
  , get
  ) where

import Prelude
import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Homogeneous (class HomogeneousRowLabels, class Keys, class ToHomogeneousRow, keysImpl)
import Data.List (catMaybes) as List
import Data.Maybe (fromJust)
import Data.Semigroup.Foldable (class Foldable1, foldMap1DefaultL, foldr1Default)
import Data.Symbol (reflectSymbol)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object) as Foreign
import Foreign.Object (empty, fromFoldable, lookup) as Foreign.Object
import Partial.Unsafe (unsafePartial)
import Prim.RowList (Cons) as RL
import Prim.RowList (class RowToList)
import Record.Unsafe (unsafeGet, unsafeSet) as Record.Unsafe
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous) as Row
import Unsafe.Coerce (unsafeCoerce)

objUnsafeGet :: forall a. String -> Foreign.Object a -> a
objUnsafeGet = unsafeCoerce Record.Unsafe.unsafeGet

objUnsafeSet :: forall a. String -> a -> Foreign.Object a -> Foreign.Object a
objUnsafeSet = unsafeCoerce Record.Unsafe.unsafeSet

newtype Homogeneous (row :: Row Type) a = Homogeneous (Foreign.Object a)

-- | The "usual" constructor when
-- | `ra` `Row` is known and you
-- | want to derive `sl` and `a`
-- | from it.
homogeneous
  :: forall a ra ls
   . HomogeneousRowLabels ra a ls
  => { | ra }
  -> Homogeneous ls a
homogeneous r =
  Homogeneous
    -- | Why this doesn't work? I have no clue.
    -- ((Object.fromHomogeneous (r ∷ { | ra })) ∷ Object a)
    (unsafeCoerce r)

-- | When you already have `Row` of labels and `a` at hand and want to derive row
-- | from them you can use this constructor instead.
homogeneous' :: forall a ra ls. ToHomogeneousRow ls a ra => Record ra -> Homogeneous ls a
homogeneous' = Homogeneous <<< unsafeCoerce

fromHomogeneous
  :: forall a ra ls
   . ToHomogeneousRow ls a ra
  => Homogeneous ls a
  -> { | ra }
fromHomogeneous (Homogeneous obj) = unsafeCoerce obj

get
  :: forall a ra ls
   . ToHomogeneousRow ls a ra
  => Homogeneous ls a
  -> ({ | ra } -> a)
  -> a
get h f = f (fromHomogeneous h)

modify
  :: forall a ra ls
   . Row.Homogeneous ra a
  => ToHomogeneousRow ls a ra
  => HomogeneousRowLabels ra a ls
  => Homogeneous ls a
  -> ({ | ra } -> { | ra })
  -> Homogeneous ls a
modify h f = homogeneous (f (fromHomogeneous h))

derive instance Eq a => Eq (Homogeneous sl a)

derive instance Ord a => Ord (Homogeneous sl a)

derive instance Functor (Homogeneous r)

instance Apply (Homogeneous r) where
  apply (Homogeneous hf) (Homogeneous ha) = Homogeneous (foldlWithIndex step Foreign.Object.empty hf)
    where
    step key result f = objUnsafeSet key (f (objUnsafeGet key ha)) result

instance (RowToList ls ll, Keys ll) => Applicative (Homogeneous ls) where
  pure a = Homogeneous obj
    where
    keys = keysImpl (Proxy :: Proxy ll)

    obj = Foreign.Object.fromFoldable <<< map (flip Tuple a) $ keys

derive newtype instance Foldable (Homogeneous r)

derive newtype instance FoldableWithIndex String (Homogeneous r)

instance (IsSymbol h, RowToList ls (RL.Cons h a tail), Keys tail) => Foldable1 (Homogeneous ls) where
  foldl1 f (Homogeneous obj) =
    let
      key = reflectSymbol (Proxy :: Proxy h)

      keys = keysImpl (Proxy :: Proxy tail)

      h = unsafePartial fromJust (Foreign.Object.lookup key obj)
    in
      foldr f h $ List.catMaybes $ map (flip Foreign.Object.lookup obj) keys
  foldr1 f = foldr1Default f
  foldMap1 f = foldMap1DefaultL f

derive newtype instance Traversable (Homogeneous r)

derive newtype instance Semigroup a => Semigroup (Homogeneous r a)

instance (RowToList ls ll, Keys ll, Monoid a) => Monoid (Homogeneous ls a) where
  mempty = pure mempty

instance Show a => Show (Homogeneous r a) where
  show (Homogeneous obj) = "Homogeneous (" <> show obj <> ")"
