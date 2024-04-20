{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "eth-core"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-formless"
  , "identity"
  , "js-bigints"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "tagged"
  , "transformers"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "app/**/*.purs" ]
}
