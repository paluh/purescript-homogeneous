# purescript-homogeneous

This library exploits the underling representation of the `Record` (`Variant` comming soon ;-)) to provide convenient instances when it is homogeneous.

## `Data.Homogeneous.Record`

The core type is:

  ```purescript
  newtype Homogeneous (row ∷ # Type) a = Homogeneous (Foreign.Object a)
  ```

`row` only provides information about the structure of a `Record` but `a` is the underling type of the values in it. "Smart construction" is done by `Foreign.Object.fromHomogeneous` which is just `unsafeCoerce` underneath. Deconstruction (`Record.Homogeneous.toRecord`) is yet another safe `unsafeCoerce` alias :-P

Given the above we can provide (by mainly newtype derving from the `Object`) many instances for this type like: `Traversable`, `Foldable`, `Monoid`. There is also an `Applicative` instance (inspired by instance from `sized-vectors`).

### Usage

Imports are required as we generate tests from this docs.

```purescript
module Test.README where

import Prelude

import Control.Comonad (extract, (=>>))
import Control.Semigroupoid (composeFlipped)
import Data.Homogeneous.Record (homogeneous, toRecord)
import Data.Homogeneous.Record (homogeneous, toRecord) as Homogeneous.Record
import Data.Variant (Variant, case_, on)
import Data.Variant (inj) as Variant
import Effect (Effect)
import Effect.Console (logShow)
import Test.Assert (assert)
import Type.Prelude (SProxy(..))
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

What this types gives us is a `Comonad` instance which allows us to `extract` `a` value from any homogeneous `Variant` easily. Additionally we have `extend` which together with `map` and `toVariant` can be probably useful to simplify repeated chain operations on a given variant.

