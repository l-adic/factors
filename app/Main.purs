module Main (main) where

import Prelude

import App.Component as App
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.Ethereum.Web3 (httpProvider)
import Prover.Verify (verifierAddress)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  provider <- liftEffect $ httpProvider "http://localhost:8545"
  Console.log $ show verifierAddress
  runUI App.component provider body