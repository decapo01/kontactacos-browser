{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut-codecs"
  , "console"
  , "effect"
  , "halogen"
  , "kontactacos-core"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
