{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "simulate-haskell-laziness-in-purescript"
, dependencies = [ "console", "effect", "psci-support", "lazy", "lists" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
