module Data.Homogeneous.Variant
  ( homogeneous
  , Homogeneous
  , toVariant
  )
  where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend)
import Data.Homogeneous.Record (ConstRow)
import Data.Variant (Variant)
import Data.Variant (on) as Variant
import Data.Variant.Internal (VariantRep(..))
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Homogeneous (row ∷ # Type) a = Homogeneous (VariantRep a)

instance functorHomogeneous ∷ Functor (Homogeneous r) where
  map f (Homogeneous (VariantRep r)) = Homogeneous (VariantRep r{ value = f r.value })

instance extendHomogeneous ∷ Extend (Homogeneous r) where
  extend f h@(Homogeneous (VariantRep r)) = Homogeneous (VariantRep (r{ value = f h}))

instance comonad ∷ Comonad (Homogeneous r) where
  extract (Homogeneous (VariantRep r)) = r.value

homogeneous ∷ ∀ a r r'. ConstRow Unit r r' (Variant r → Homogeneous r' a)
homogeneous = Homogeneous <<< unsafeCoerce

toVariant ∷ ∀ a r r'. ConstRow a r r' (Homogeneous r a → Variant r')
toVariant (Homogeneous v) = unsafeCoerce v

default ∷ ∀ a b r. (a → b) → Homogeneous r a → b
default f = f <<< extract

-- on
--   ∷ ∀ sym a b r1 r1a r2 r2a
--   . Row.Cons sym Unit r1 r2
--   ⇒ Row.Cons sym a r1a r2a
--   ⇒ IsSymbol sym
--   ⇒ ConstRow a r1 r1a
--     (SProxy sym
--     → (a → b)
--     → (Homogeneous r1 a → b)
--     → Homogeneous r2 a
--     → b
--     )
-- on p f g r = Variant.on p f (g <<< homogeneous) (toVariant r ∷ Variant r1a)

--   case coerceV r of
--     VariantRep v | v.type == reflectSymbol p → f v.value
--     _ → g (coerceR r)
--   where
--   coerceV ∷ Variant r2 → VariantRep a
--   coerceV = unsafeCoerce
-- 
--   coerceR ∷ Variant r2 → Variant r1
--   coerceR = unsafeCoerce
