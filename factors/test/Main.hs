module Main (main) where

import Circuit (lookupVar)
import Circuit.Solver.Circom (CircomProgram (..), nativeGenWitness)
import qualified Data.Map as Map
import Protolude
import R1CS (Witness (..))
import R1CS.Circom (witnessFromCircomWitness)
import Test.Hspec
import Test.QuickCheck
import ZK.Factors (factors)

main :: IO ()
main = hspec $ do
  let vars = cpVars factors
  describe "Factors" $ do
    it "should accept valid factors" $ do
      property $
        \x y ->
          let inputs = Map.fromList [("n", x * y), ("a", x), ("b", y)]
              Witness w =
                witnessFromCircomWitness $
                  nativeGenWitness factors inputs
           in lookupVar vars "out" w === Just 1
    it "shouldn't accept invalid factors" $ do
      property $
        \x y z ->
          (x * y /= z) ==>
            let inputs = Map.fromList [("n", z), ("a", x), ("b", y)]
                Witness w =
                  witnessFromCircomWitness $
                    nativeGenWitness factors inputs
             in lookupVar vars "out" w == Just 0
