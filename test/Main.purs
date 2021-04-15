module Test.Main where

import Prelude
import Data.Array (head) as Array
import Data.Homogeneous.Record (homogeneous, fromHomogeneous) as Record
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Assert (assert)
import Test.README (recordInstances)

main :: Effect Unit
main = do
  let
    recHeads rec = Record.fromHomogeneous (map Array.head (Record.homogeneous rec))
  assert $ recHeads { a: [ 1 ], b: [ 2, 3 ], c: [] } == { a: Just 1, b: Just 2, c: Nothing }
  recordInstances
 -- variantInstances
