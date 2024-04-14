module Main where

import Data.Binary (encodeFile)
import Protolude
import R1CS (toR1CS)
import R1CS.Circom (r1csToCircomR1CS)
import ZK.Factors (FactorsCircuit (..), Fr, factorsCircuit)

main :: IO ()
main = do
  let r1cs = toR1CS $ fcCircuit @Fr factorsCircuit
  encodeFile "factors-output/factors.r1cs" $ r1csToCircomR1CS r1cs
