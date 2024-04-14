module Main (main) where

import qualified Data.Map as Map
import Protolude
import R1CS (Witness (Witness))
import Test.Hspec
import Test.QuickCheck
import ZK.Factors (FactorsCircuit (..), Fr, factorsCircuit, solver)

main :: IO ()
main = hspec $ do
  describe "Factors" $ do
    let output = fcOutput $ factorsCircuit @Fr
    it "should accept valid factors" $ do
      property $
        \x y ->
          let Witness w = solver @Fr (x * y) (x, y)
           in Map.lookup output w == Just 1
    it "shouldn't accept invalid factors" $ do
      property $
        \x y z ->
          (x * y /= z) ==>
            let Witness w = solver @Fr z (x, y)
             in Map.lookup output w == Just 0
