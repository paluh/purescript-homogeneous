module Data.Homogeneous where

import Prelude

import Data.List (List, (:))
import Prim.Row (class Cons) as Row
import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, RowList)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Type.Row.Homogeneous (class Homogeneous) as Row

-- | Fold a `RowList` into a row but use `a` type
-- | to fill all values type.
class ListToHomogeneous :: forall k. RowList Type -> k -> Row Type -> Constraint
class ListToHomogeneous (rl :: RowList Type) a (r :: Row Type) | rl a -> r

instance ListToHomogeneous RL.Nil a ()

instance (Row.Cons l a ls_ ls, ListToHomogeneous t a ls_) => ListToHomogeneous (RL.Cons l b t) a ls

-- | We provide two different versions of constraints
-- | so you can get your homogeneous row
-- | from labels and value type alone and you can get
-- | labels and value type just from row.
-- | This can be useful when we don't know the type
-- | of the row yet etc.
class HomogeneousRowLabels :: forall a b. Row a -> a -> Row b -> Constraint
class HomogeneousRowLabels r a ls | r -> a ls

instance (RowToList r rl, ListToHomogeneous rl Void ls, Row.Homogeneous r a) => HomogeneousRowLabels r a ls

class ToHomogeneousRow :: forall a b. Row a -> b -> Row b -> Constraint
class ToHomogeneousRow ls b r | ls b -> r

instance (RowToList ls ll, ToHomogeneousRow' ll a r) => ToHomogeneousRow ls a r

class ToHomogeneousRow' :: forall a b. RowList a -> b -> Row b -> Constraint
class ToHomogeneousRow' ll b r | ll b -> r

instance ToHomogeneousRow' RL.Nil a ()
else instance (Row.Cons h a r_ r, ToHomogeneousRow' t a r_) => ToHomogeneousRow' (RL.Cons h b t) a r

-- | Reflect row list labels into a `List String`.
-- | Taken from `record-extra`.
class Keys (xs :: RowList Type) where
  keysImpl :: Proxy xs -> List String

instance Keys RL.Nil where
  keysImpl _ = mempty

instance
  ( IsSymbol name
  , Keys tail
  ) =>
  Keys (RL.Cons name ty tail) where
  keysImpl _ = first : rest
    where
    first = reflectSymbol (Proxy :: Proxy name)

    rest = keysImpl (Proxy :: Proxy tail)
