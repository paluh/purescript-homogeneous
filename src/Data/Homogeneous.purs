module Data.Homogeneous where

import Prelude
import Prim.Row (class Cons) as Row
import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Type.Row.Homogeneous (class Homogeneous) as Row

-- | Fold a `RowList` into a row but use `a` type
-- | to fill all values type.
class ListToHomogeneous (rl ∷ RowList) a (r ∷ # Type) | rl a → r

instance foldHomogeneousNil ∷ ListToHomogeneous RL.Nil a ()

instance foldHomogeneousCons ∷ (Row.Cons l a ls_ ls, ListToHomogeneous t a ls_) ⇒ ListToHomogeneous (RL.Cons l b t) a ls

-- | We provide two different versions of constraints
-- | so you can get your homogeneous row
-- | from labels and value type alone and you can get
-- | labels and value type just from row.
-- | This can be useful when we don't know the type
-- | of the row yet etc.
class HomogeneousRowLabels (r ∷ # Type) a (ls ∷ # Type) | r → a ls

instance rowSlist ∷ (RowToList r rl, ListToHomogeneous rl Void ls, Row.Homogeneous r a) ⇒ HomogeneousRowLabels r a ls

class ToHomogeneousRow (ls ∷ # Type) a (r ∷ # Type) | ls a → r

instance labelsToRow ∷ (RowToList ls ll, ToHomogeneousRow' ll a r) ⇒ ToHomogeneousRow ls a r

class ToHomogeneousRow' (ll ∷ RowList) a (r ∷ # Type) | ll a → r

instance labelsToRowNil ∷ ToHomogeneousRow' RL.Nil a ()
else instance labelsToRowCons ∷ (Row.Cons h a r_ r, ToHomogeneousRow' t a r_) ⇒ ToHomogeneousRow' (RL.Cons h b t) a r
