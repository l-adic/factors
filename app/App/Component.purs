module App.Component where

import Prelude

import App.Form as Form
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Effect.Aff (Aff, attempt)
import Effect.Class.Console as Console
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Network.Ethereum.Web3 (Provider, runWeb3)
import Partial.Unsafe (unsafePartial)
import Prover.Prove (fullProve)
import Prover.Types (Fp(..), Inputs(..))
import Prover.Verify (verifierAddress, verify)
import Type.Proxy (Proxy(..))

type CircuitInput = { factorA :: BigInt, factorB :: BigInt, product :: BigInt }

type State =
  { circuitInput :: Maybe CircuitInput
  , provider :: Provider
  , message :: Maybe String
  }

type Input = Provider

data Action = HandleCircuitInput CircuitInput

type Slots = (circuitInput :: forall query. H.Slot query CircuitInput Unit)

_circuitInput = Proxy :: Proxy "circuitInput"

component :: forall query output. H.Component query Input output Aff
component =
  H.mkComponent
    { initialState: mkInitialState
    , render
    , eval: H.mkEval H.defaultEval
        { initialize = Nothing
        , handleAction = handleAction
        }
    }
  where
  mkInitialState :: Input -> State
  mkInitialState provider =
    { circuitInput: Nothing
    , provider: provider
    , message: Nothing
    }

  render st =
    HH.div_
      [ HH.slot _circuitInput 0 Form.component unit HandleCircuitInput
      , HH.div_ [ HH.text $ fromMaybe "" st.message ]
      , HH.div_ [ HH.text $ "Verifier Contract: " <> show verifierAddress ]
      ]

  handleAction = case _ of
    HandleCircuitInput ci -> do
      H.modify_ _ { circuitInput = Just ci, message = Nothing }
      eProof <- liftAff $ attempt $
        fullProve { a: ci.factorA, b: ci.factorB, n: ci.product }
      case eProof of
        Left e -> do
          Console.log $ "Prover Error: " <> show e
          let msg = "Prover Error, check console logs for details."
          H.modify_ _ { message = Just msg }
        Right { inputs, proof } -> do
          { provider } <- H.get
          eRes <- liftAff $ runWeb3 provider $ verify { inputs, proof }
          let
            msg = case eRes of
              Left e -> "Web3 Error: " <> show e
              Right res ->
                case res of
                  Left callError -> "Call Error " <> show callError
                  -- this contract reverts if it doesn't terminate with true
                  Right _ -> 
                    let ins = unsafePartial $ fromJust do
                          let Inputs is = inputs  
                          out <- is !! 0
                          n <- is !! 1
                          -- the output is a field encoded boolean value
                          pure {out: out == Fp (BigInt.fromInt 1), n}
                    in "Proof validated by conract with public inputs " <> show ins
          H.modify_ _ { message = Just msg }