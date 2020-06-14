module Data.Homogeneous.Record where

import Prelude

import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex)
import Data.Homogeneous (class Fill)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object (empty, fromFoldable, fromHomogeneous) as Object
import Prim.RowList (class RowToList)
import Record.Extra (class Keys, keysImpl) as Record.Extra
import Record.Unsafe (unsafeGet, unsafeSet) as Record.Unsafe
import Type.Prelude (RLProxy(..))
import Type.Row.Homogeneous (class Homogeneous) as Row
import Unsafe.Coerce (unsafeCoerce)

objUnsafeGet ∷ ∀ a. String → Object a → a
objUnsafeGet = unsafeCoerce Record.Unsafe.unsafeGet

objUnsafeSet ∷ ∀ a. String → a → Object a → Object a
objUnsafeSet = unsafeCoerce Record.Unsafe.unsafeSet

newtype Homogeneous (row ∷ # Type) a = Homogeneous (Object a)

derive instance functorHomogeneous ∷ Functor (Homogeneous r)
instance applyHomogeneousRecord ∷ Apply (Homogeneous r) where
  apply (Homogeneous hf) (Homogeneous ha) = Homogeneous (foldlWithIndex step Object.empty hf)
    where
      step key result f = objUnsafeSet key (f (objUnsafeGet key ha)) result
instance applicativeHomogeneousRecord ∷ (Record.Extra.Keys rl, RowToList r rl) ⇒ Applicative (Homogeneous r) where
  pure a = Homogeneous obj
    where
      keys = Record.Extra.keysImpl (RLProxy ∷ RLProxy rl)
      obj = Object.fromFoldable <<< map (flip Tuple a) $ keys
derive newtype instance foldableHomogeneous ∷ Foldable (Homogeneous r)
derive newtype instance foldableWithIndexHomogeneous ∷ FoldableWithIndex String (Homogeneous r)
derive newtype instance traversableHomogeneous ∷ Traversable (Homogeneous r)
derive newtype instance semigroupHomogeneous ∷ Semigroup a ⇒ Semigroup (Homogeneous r a)
instance monoidHomogeneous ∷ (Record.Extra.Keys rl, RowToList r rl, Monoid a) ⇒ Monoid (Homogeneous r a) where
  mempty = pure mempty

homogeneous
  ∷ ∀ a ra ru
  . Row.Homogeneous ra a
  ⇒ Fill Unit ra ru
  ⇒ { | ra }
  → Homogeneous ru a
homogeneous = Homogeneous <<< Object.fromHomogeneous

toRecord
  ∷ ∀ a ra ru
  . Fill a ru ra
  ⇒ Homogeneous ru a
  → { | ra }
toRecord (Homogeneous obj) = unsafeCoerce obj

get
  ∷ ∀ a ra ru
  . Fill a ru ra
  ⇒ Homogeneous ru a
  → ({ | ra } → a)
  → a
get h f = f (toRecord h)

modify
  ∷ ∀ a ra ru. Row.Homogeneous ra a
  ⇒ Fill Unit ra ru
  ⇒ Fill a ru ra
  ⇒ Homogeneous ru a
  → ({ | ra } → { | ra })
  → Homogeneous ru a
modify h f = homogeneous (f (toRecord h))
