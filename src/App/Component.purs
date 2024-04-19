module App.Component where

import Prelude

import App.Form as Form
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import JS.BigInt (BigInt)
import Prover.Prove (fullProve)
import Type.Proxy (Proxy(..))

type CircuitInput = { factorA :: BigInt, factorB :: BigInt, product :: BigInt }

type State =
  { circuitInput :: Maybe CircuitInput
  , error :: Maybe String
  }

type Input = Unit

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
  mkInitialState _ =
    { circuitInput: Nothing
    , error: Nothing
    }

  render _ =
    HH.div_
      [ HH.slot _circuitInput 0 Form.component unit HandleCircuitInput
      ]

  handleAction = case _ of
    HandleCircuitInput ci -> do
      H.modify_ _ { circuitInput = Just ci }
      proof <- liftAff $ fullProve { a: ci.factorA, b: ci.factorB, n: ci.product }
      liftEffect $ Console.log $ show proof