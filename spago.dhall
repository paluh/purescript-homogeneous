{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ license = "BSD-3-Clause"
, dependencies =
  [ "arrays"
  , "assert"
  , "control"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "foreign-object"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, name = "homogeneous"
, packages = ./packages.dhall
, repository = "https://github.com/paluh/purescript-homogeneous.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
