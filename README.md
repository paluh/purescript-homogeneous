# purescript-homogeneous

This library exploits the underling representation of the `Record` and `Variant` to provide a convenient instances when they are homogeneous (all fields or all cases have the same type).

## `Data.Homogeneous.Record`

The core type ~~is~~ was:

  ```purescript
  newtype Homogeneous (row ∷ # Type) a = Homogeneous (Foreign.Object a)
  ```

now it is (**):

  ```purescript
  newtype Homogeneous (slist ∷ SList) a = Homogeneous (Foreign.Object a)
  ```

`SList` provides information about the structure of the `Record` but `a` is the underling type of the values in it. "Smart construction" is done by `Foreign.Object.fromHomogeneous` which is just `unsafeCoerce` underneath. Deconstruction (`Record.Homogeneous.toRecord`) is yet another safe `unsafeCoerce` alias :-P

Given the above we can provide (by mainly newtype derving from the `Object`) many instances for this type like: `Traversable`, `Foldable`, `Monoid`. There is also an `Applicative` instance (inspired by instance from `sized-vectors`).

(**) I've moved to `SList` (heterogeneous list of `Symbol`s) to represent the structure of the given record because it has better inference properties. This is not ideal because when you want to specify label set by hand you should provide them in alphabetical order... ;-)


### Installation

`puresrcipt-homogeneous` is not in the package set, and neither is its dependency `purescript-typelevel-eval`. So for now, you'll need to manually add them to your project's `packages.dhall`.

  ```
  let additions =
    { homogeneous =
      { dependencies =
        [ "foreign-object", "record-extra", "typelevel-eval", "variant" ]
      , repo = "https://github.com/paluh/purescript-homogeneous.git"
      , version = "master"
      }
    }
  ```


### Usage

Imports are required as we generate tests from this docs.

```purescript
module Test.README where

import Prelude

import Data.Homogeneous.Record (homogeneous, toRecord) as Homogeneous.Record
import Effect (Effect)
import Test.Assert (assert)
```

Let's create two records. The first is our data record the second one (created with `pure`) is a record full of functions (identical in this case `_ * 2`):

```purescript

recordInstances :: Effect Unit
recordInstances = do
  let
    r = { one: 1, two: 2, three: 3 }

    multiply = pure (_ * 2)

```

To use instances from `Homogeneous.Record` we need to wrap our record using a smart constructor `homogeneous`. To go back we unwrap it using `toRecord`. Both functions are really "cheap" underneath.

```purescript
    hr = Homogeneous.Record.homogeneous r

    o = multiply <*> hr

    r' = Homogeneous.Record.toRecord o

  assert (r' == { one: 2, two: 4, three: 6 })

```

Of course we have others instances at hand too:

```purescript
  assert
    ((Homogeneous.Record.toRecord <<< map show $ hr) == { one: "1", two: "2", three: "3" })
```

What is quite a nice about this `newtype` approach is that underling machinery (we deriving nearly all instances) is really simple and efficient based on `Foreign.Object`.


## `Data.Homogeneous.Variant`

The core type is:

  ```purescript
  newtype Homogeneous (row ∷ # Type) a = Homogeneous (VariantRep a)
  ```

What this type gives us is a `Comonad` instance which allows us to `extract` `a` value from any homogeneous `Variant` easily. Additionally we have `extend` which together with `map` and `toVariant` can be probably useful to simplify repeated chain operations on a given variant.


<!--
-- ```purescript
-- type TrianglePoint = Variant (p1 ∷ { x ∷ Number, y ∷ Number }, p2 ∷ String)
-- 
-- variantInstances ∷ Effect Unit
-- variantInstances = do
--   let
--     -- mul' = composeFlipped Homogeneous.Variant.toVariant $ case_
--     --   # on (SProxy ∷ SProxy "one") (mul 1)
--     --   # on (SProxy ∷ SProxy "two") (mul 2)
-- 
--     o = Homogeneous.Variant.homogeneous ((Variant.inj (SProxy ∷ SProxy "one") 1) ∷ Numbers
--     t = Homogeneous.Variant.homogeneous ((Variant.inj (SProxy ∷ SProxy "two") 2) ∷ Variant (two ∷ Int, one ∷ Int))
-- 
--     t' = t =>> mul'
--     o' = o =>> mul'
-- 
--   logShow $ extract t'
--   logShow $ extract o'
-- ```
-- 
-->
