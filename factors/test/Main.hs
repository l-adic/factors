module Main (main) where

import Circuit (CircuitVars (..), solve)
import qualified Data.Map as Map
import Protolude
import Test.Hspec
import Test.QuickCheck
import ZK.Factors (Factors (..), Fr, factors)

main :: IO ()
main = hspec $ do
  let circuit = factorsCircuit $ factors @Fr
      vars = factorsVars $ factors @Fr
      (n, a, b, out) = fromMaybe (panic "Inputs not found") $ do
        _n <- Map.lookup "n" $ cvInputsLabels vars
        _a <- Map.lookup "a" $ cvInputsLabels vars
        _b <- Map.lookup "b" $ cvInputsLabels vars
        _out <- Map.lookup "out" $ cvInputsLabels vars
        pure (_n, _a, _b, _out)
  describe "Factors" $ do
    it "should accept valid factors" $ do
      property $
        \x y ->
          let inputs = Map.fromList [(n, x * y), (a, x), (b, y), (out,1)]
              w = solve vars circuit inputs
           in Map.lookup out w == Just 1
    it "shouldn't accept invalid factors" $ do
      property $
        \x y z ->
          (x * y /= z) ==>
            let inputs = Map.fromList [(n, z), (a, x), (b, y)]
                w = solve vars circuit inputs
             in Map.lookup out w == Just 0
