module Main (main) where

import Circom.R1CS (witnessFromCircomWitness)
import Circom.Solver (CircomProgram (..), mkCircomProgram, nativeGenWitness)
import Circuit
import Circuit.Language
import qualified Data.Map as Map
import Protolude
import R1CS (Witness (..))
import Test.Hspec
import Test.QuickCheck
import ZK.Factors (Fr, factors)
import Data.Binary (encode, decode)

main :: IO ()
main = hspec $ do
  let BuilderState {bsVars, bsCircuit} = snd $ runCircuitBuilder (factors @Fr)
      program = mkCircomProgram bsVars bsCircuit
      vars = cpVars program
  describe "Factors" $ do
    it "can serialize/deserialize the program" $ do
      let a = decode (encode program)
      cpCircuit a `shouldBe` cpCircuit program
      
    it "should accept valid factorizations" $
      property $
        \x y ->
          (x /= 1 && y /= 1) ==>
            let inputs = Map.fromList [("n", Simple $ x * y), ("a", Simple x), ("b", Simple y)]
                Witness w =
                  witnessFromCircomWitness $
                    nativeGenWitness program inputs
             in lookupVar vars "out" w === Just 1
    it "shouldn't accept trivial factorizations" $
      property $ \x ->
        let inputs = Map.fromList [("n", Simple x), ("a", Simple 1), ("b", Simple x)]
            Witness w =
              witnessFromCircomWitness $
                nativeGenWitness program inputs
         in lookupVar vars "out" w == Just 0
    it "shouldn't accept invalid factorizations" $
      property $
        \x y z ->
          (x * y /= z) ==>
            let inputs = Map.fromList [("n", Simple z), ("a", Simple x), ("b", Simple y)]
                Witness w =
                  witnessFromCircomWitness $
                    nativeGenWitness program inputs
             in lookupVar vars "out" w == Just 0
