module App.Component where

import Prelude

import App.Form as Form
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff, attempt)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import JS.BigInt (BigInt)
import Network.Ethereum.Web3 (Provider, runWeb3)
import Prover.Prove (fullProve)
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
        Left _ -> do
          let msg = "Prover Error, are you sure the statement is true?"
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
                  Right b -> "Proof validated by conract: " <> show b
          H.modify_ _ { message = Just msg }