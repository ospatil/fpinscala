{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "fpinscala"
, dependencies = [ "arrays", "console", "effect", "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
