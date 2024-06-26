let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240416/packages.dhall
        sha256:ca727657c01cc31d0e79c2113b59126b9826f4b56d20a8193be3c725599fb754

let web3-deps =
      https://raw.githubusercontent.com/f-o-a-m/chanterelle/v7.0.0-rc6/packages.dhall
        sha256:56c0e5b02ed94186f5263e6da800bd1cc7f09fd5f6a9ab1e5f8c91ef23f5a297

let additions =
      { chanterelle =
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
      , solc = web3-deps.solc
      , tidy = web3-deps.tidy
      , tidy-codegen = web3-deps.tidy-codegen
      , web3 = web3-deps.web3
      , web3-generator = web3-deps.web3-generator
      }

let overrides =
      { node-buffer =
        { dependencies =
          [ "arraybuffer-types"
          , "assert"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "functions"
          , "maybe"
          , "nullable"
          , "partial"
          , "prelude"
          , "st"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/l-adic/purescript-node-buffer.git"
        , version = "12aaa4a98d2aadd506261c8bed051cee0e49b0ba"
        }
      }

in  upstream // overrides // additions
