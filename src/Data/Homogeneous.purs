module Data.Homogeneous where

import Prim.Row (class Cons) as Row
import Prim.RowList (class RowToList)
import Record.Extra (class SListToRowList, type (:::), SNil, kind SList)
import Type.Prelude (class TypeEquals, RProxy)
import Type.Row.Homogeneous (class Homogeneous) as Row

-- | We provide two different versions of constraints
-- | so you can get your homogeneous row
-- | from labels and value type alone and you can get
-- | labels and value type just from row.
-- | This can be useful when we don't know the type
-- | of the row yet etc.
class (Row.Homogeneous r a) ⇐ RowSList (sl ∷ SList) a (r ∷ # Type) | r → a sl

instance rowSlist ∷ (SListToRowList sl rl, Row.Homogeneous r a, RowToList r rl) ⇒ RowSList sl a r

class SListRow (sl ∷ SList) a (r ∷ # Type) | sl a → r

instance slistRowNil ∷ (TypeEquals (RProxy ()) (RProxy r)) ⇒ SListRow SNil a r
else instance slistRowCons ∷ (Row.Cons h a r_ r, SListRow t a r_) ⇒ SListRow (h ::: t) a r
