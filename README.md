# purescript-homogeneous

This library exploits the underling representation of the `Record` (`Variant` comming soon ;-)) to provide convenient instances when it is homogeneous.

## `Data.Homogeneous.Record`

The core type is:

```purescript
newtype Homogeneous (row ∷ # Type) a = Homogeneous (Foreign.Object a)
```

`row` only provides information about the structure of a `Record` but `a` is the underling type of the values in it. "Smart construction" is done by `Foreign.Object.fromHomogeneous` which is just `unsafeCoerce` underneath. Deconstruction (`Record.Homogeneous.toRecord`) is yet another safe `unsafeCoerce` alias :-P

Given the above we can provide (by mainly newtype derving from the `Object`) many instances for this type like: `Traversable`, `Foldable`, `Monoid`. There is also an `Applicative` instance (which is isomorphic to the instance from `sized-vectors`) which allows us to do:

```purescript

main :: Effect Unit
main = do
  let
    r = { one: 1, two: 2, three: 3 }

    multiply = pure (_ * 2)

    -- | We put a record in
    o = multiply <*> Homogeneous.Record.homogeneous r

    -- | We get a record back
    r' = Homogeneous.Record.toRecord o

  logShow r'
```

which outputs:

```shell
{ one: 2, three: 6, two: 4 }
```

What is quite a nice about this `newtype` approach is that underling machinery for deriving all instances is really simple and efficient :-) Additionally it seems that it can be complemented by `Homogeneous.Variant` in the future...


## `Data.Homogeneous.Variant`

The core type is:

```purescript
newtype Homogeneous (row ∷ # Type) a = Homogeneous (VariantRep a)
```

What this types gives us is a `Comonad` instance which allows us to `extract` `a` value from any homogeneous `Variant` easily. Additionally we have `extend` which together with `map` and `toVariant` can be probably useful to simplify repeated chain operations on a given variant.

