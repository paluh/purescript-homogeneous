# purescript-homogeneous

This library exploits the underling representation of `Record` a bit to provide convenient instances when they are homogeneous.

## `Data.Homogeneous.Record`

The core type is:

```
newtype Homogeneous (row ∷ # Type) a = Homogeneous (Object a)
```

`row` only provides information about the structure of the given `Record` (we put `Unit` as a placeholder for values) but `a` is the underling type of values in it.

Given the above we can provide (by mainly newtype derving from the `Object`) many instances for this type like: `Traversable`, `Foldable`, `Monoid`. There is also `Applicative` (which is isomorphic to the instance from `sized-vectors`) which allows us to do:

```purescript

main :: Effect Unit
main = do
  let
    i = Homogeneous.Record.homogeneous { one: 1, two: 2, three: 3 }

    multiply = pure (_ * 2)

    o = multiply <*> h

  logShow (Homogeneous.Record.toRecord o)
```
which outputs:

```shell
{ one: 2, three: 6, two: 4 }
```
