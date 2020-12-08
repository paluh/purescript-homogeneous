module Data.Homogeneous where

import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import Record.Extra (SCons, SNil, kind SList)
import Type.Prelude (class ListToRow)
import Type.Row.Homogeneous (class Homogeneous) as Row

class SListRowListIso (sl ∷ SList) a (rl ∷ RowList) | rl → a sl, sl a → rl

instance slistRowIsoNil :: SListRowListIso SNil a RL.Nil
else instance slistRowListIsoCons ::
  ( SListRowListIso sTail a tail
    ) =>
  SListRowListIso (SCons name sTail) a (RL.Cons name a tail)

class (Row.Homogeneous r a) ⇐ RowSList (sl ∷ SList) a (r ∷ #Type) | r → a sl, sl a → r

instance rowSlist ∷ (Row.Homogeneous r a, RowToList r rl, SListRowListIso sl a rl) ⇒ RowSList sl a r

class (Row.Homogeneous r a) ⇐ SListRow (sl ∷ SList) a r | r → a sl, sl a → r

instance slistRow ∷ (Row.Homogeneous r a, ListToRow rl r, SListRowListIso sl a rl) ⇒ SListRow sl a r
