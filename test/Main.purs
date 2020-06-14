module Test.Main where

import Prelude

import Effect (Effect)
import Test.README (recordInstances)

main :: Effect Unit
main = do
  recordInstances

--   let
--     mul' = composeFlipped Homogeneous.Variant.toVariant $ case_
--       # on (SProxy ∷ SProxy "one") (mul 1)
--       # on (SProxy ∷ SProxy "two") (mul 2)
-- 
--     o = Homogeneous.Variant.homogeneous ((Variant.inj (SProxy ∷ SProxy "one") 1) ∷ Variant (two ∷ Int, one ∷ Int))
--     t = Homogeneous.Variant.homogeneous ((Variant.inj (SProxy ∷ SProxy "two") 2) ∷ Variant (two ∷ Int, one ∷ Int))
-- 
--     t' = t =>> mul'
--     o' = o =>> mul'
-- 
--   logShow $ extract t'
--   logShow $ extract o'
