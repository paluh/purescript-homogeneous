module Data.Homogeneous where

import Type.Eval (class Eval) as Type.Eval
import Type.Eval.Function (Const) as Type.Eval
import Type.Eval.Function (type (<<<))
import Type.Eval.Functor (Map) as Type.Eval
import Type.Eval.RowList (FromRow, ToRow) as Type.Eval
import Type.Row (RProxy) as Row

type ConstRow a = Type.Eval.ToRow <<< Type.Eval.Map (Type.Eval.Const a) <<< Type.Eval.FromRow

-- | Handy alias
class Fill a (r ∷ # Type) (r' ∷ # Type) | a r → r'
instance fill ∷ (Type.Eval.Eval (ConstRow a (Row.RProxy r)) (Row.RProxy r')) ⇒ Fill a r r'


