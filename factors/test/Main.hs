module Main (main) where

import Circuit (solve, lookupVar, assignInputs)
import qualified Data.Map as Map
import Protolude
import Test.Hspec
import Test.QuickCheck
import ZK.Factors (Factors (..), Fr, factors)

main :: IO ()
main = hspec $ do
  let circuit = factorsCircuit $ factors @Fr
      vars = factorsVars $ factors @Fr
  describe "Factors" $ do
    it "should accept valid factors" $ do
      property $
        \x y ->
          let inputs = assignInputs vars $ Map.fromList [("n", x * y), ("a", x), ("b", y)]
              w = solve vars circuit inputs
          in lookupVar vars "out" w === Just 1
    it "shouldn't accept invalid factors" $ do
      property $
        \x y z ->
          (x * y /= z) ==>
            let inputs = assignInputs vars $ Map.fromList [("n", z), ("a", x), ("b", y)]
                w = solve vars circuit inputs
            in lookupVar vars "out" w == Just 0
