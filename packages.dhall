let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240416/packages.dhall
        sha256:ca727657c01cc31d0e79c2113b59126b9826f4b56d20a8193be3c725599fb754

let web3-deps =
      https://raw.githubusercontent.com/f-o-a-m/chanterelle/master/packages.dhall
        sha256:430e33712dea8927565a2efdff4f91b02dc3840fce000a1b8a7d82876c00ee80

let additions =
      { bytestrings = web3-deps.bytestrings
      , chanterelle =
        { dependencies =
          [ "aff"
          , "ansi"
          , "argonaut"
          , "argonaut-core"
          , "argonaut-traversals"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "console"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "eth-core"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "functors"
          , "identity"
          , "integers"
          , "js-date"
          , "logging"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-fs"
          , "node-path"
          , "node-process"
          , "now"
          , "optparse"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "prelude"
          , "profunctor-lenses"
          , "record"
          , "refs"
          , "simple-json"
          , "solc"
          , "strings"
          , "transformers"
          , "tuples"
          , "unfoldable"
          , "validation"
          , "web3"
          , "web3-generator"
          ]
        , repo = "https://github.com/f-o-a-m/chanterelle.git"
        , version = "v7.0.0-rc6"
        }
      , coroutine-transducers = web3-deps.coroutine-transducers
      , dodo-printer = web3-deps.dodo-printer
      , eth-core = web3-deps.eth-core
      , quotient = web3-deps.quotient
      , solc = web3-deps.solc
      , tidy = web3-deps.tidy
      , tidy-codegen = web3-deps.tidy-codegen
      , web3 = web3-deps.web3
      , web3-generator = web3-deps.web3-generator
      }

in  upstream // additions
