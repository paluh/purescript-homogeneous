{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ license = "BSD-3-Clause"
, dependencies =
    [ "assert"
    , "console"
    , "effect"
    , "foreign-object"
    , "psci-support"
    , "record-extra"
    , "variant"
    ]
, name = "homogeneous"
, packages = ./packages.dhall
, repository = "https://github.com/paluh/purescript-homogeneous.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
