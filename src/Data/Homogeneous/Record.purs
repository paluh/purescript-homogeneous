module Data.Homogeneous.Record where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Homogeneous (class RowSList, class SListRow)
import Data.List (catMaybes) as List
import Data.Maybe (fromJust)
import Data.Semigroup.Foldable (class Foldable1, foldMap1Default)
import Data.Symbol (reflectSymbol)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object) as Foreign
import Foreign.Object (empty, fromFoldable, lookup) as Foreign.Object
import Partial.Unsafe (unsafePartial)
import Record.Extra (class Keys, class SListToRowList, type (:::), SLProxy(..), kind SList)
import Record.Extra (slistKeys) as Record.Extra
import Record.Unsafe (unsafeGet, unsafeSet) as Record.Unsafe
import Type.Prelude (class IsSymbol, SProxy(..))
import Type.Row.Homogeneous (class Homogeneous) as Row
import Unsafe.Coerce (unsafeCoerce)

objUnsafeGet ∷ ∀ a. String → Foreign.Object a → a
objUnsafeGet = unsafeCoerce Record.Unsafe.unsafeGet

objUnsafeSet ∷ ∀ a. String → a → Foreign.Object a → Foreign.Object a
objUnsafeSet = unsafeCoerce Record.Unsafe.unsafeSet

newtype Homogeneous (row ∷ SList) a
  = Homogeneous (Foreign.Object a)

homogeneous ∷
  ∀ a ra sl.
  RowSList sl a ra ⇒
  { | ra } →
  Homogeneous sl a
homogeneous r =
  Homogeneous
    -- | Why this doesn't work? I have no clue.
    -- ((Object.fromHomogeneous (r ∷ { | ra })) ∷ Object a)
    (unsafeCoerce r)

toRecord ∷
  ∀ a ra sl.
  SListRow sl a ra ⇒
  Homogeneous sl a →
  { | ra }
toRecord (Homogeneous obj) = unsafeCoerce obj

get ∷
  ∀ a ra sl.
  SListRow sl a ra ⇒
  Homogeneous sl a →
  ({ | ra } → a) →
  a
get h f = f (toRecord h)

modify ∷
  ∀ a ra slist.
  Row.Homogeneous ra a ⇒
  SListRow slist a ra ⇒
  RowSList slist a ra ⇒
  Homogeneous slist a →
  ({ | ra } → { | ra }) →
  Homogeneous slist a
modify h f = homogeneous (f (toRecord h))

derive instance eqHomogeneous ∷ Eq a ⇒ Eq (Homogeneous sl a)

derive instance ordHomogeneous ∷ Ord a ⇒ Ord (Homogeneous sl a)

derive instance genericHomogeneous ∷ Generic (Homogeneous sl a) _

derive instance functorHomogeneous ∷ Functor (Homogeneous r)

instance applyHomogeneousRecord ∷ Apply (Homogeneous r) where
  apply (Homogeneous hf) (Homogeneous ha) = Homogeneous (foldlWithIndex step Foreign.Object.empty hf)
    where
    step key result f = objUnsafeSet key (f (objUnsafeGet key ha)) result

instance applicativeHomogeneousRecord ∷ (SListToRowList slist rl, Keys rl) ⇒ Applicative (Homogeneous slist) where
  pure a = Homogeneous obj
    where
    keys = Record.Extra.slistKeys (SLProxy ∷ SLProxy slist)

    obj = Foreign.Object.fromFoldable <<< map (flip Tuple a) $ keys

derive newtype instance foldableHomogeneous ∷ Foldable (Homogeneous r)

derive newtype instance foldableWithIndexHomogeneous ∷ FoldableWithIndex String (Homogeneous r)

instance foldable1Homogeneous ∷ (IsSymbol h, SListToRowList tail rl, Keys rl) ⇒ Foldable1 (Homogeneous (h ::: tail)) where
  fold1 (Homogeneous obj) =
    let
      key = reflectSymbol (SProxy ∷ SProxy h)

      keys = Record.Extra.slistKeys (SLProxy ∷ SLProxy tail)

      h = unsafePartial fromJust (Foreign.Object.lookup key obj)
    in
      foldr append h $ List.catMaybes $ map (flip Foreign.Object.lookup obj) keys
  foldMap1 f = foldMap1Default f

derive newtype instance traversableHomogeneous ∷ Traversable (Homogeneous r)

derive newtype instance semigroupHomogeneous ∷ Semigroup a ⇒ Semigroup (Homogeneous r a)

instance monoidHomogeneous ∷ (Keys rl, SListToRowList slist rl, Monoid a) ⇒ Monoid (Homogeneous slist a) where
  mempty = pure mempty

instance showHomogeneous ∷ Show a ⇒ Show (Homogeneous r a) where
  show (Homogeneous obj) = "Homogeneous (" <> show obj <> ")"
