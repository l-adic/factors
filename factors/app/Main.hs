module Main where

import Data.Binary (encodeFile)
import Protolude
import R1CS (toR1CS)
import R1CS.Circom (r1csToCircomR1CS)
import Circuit.Solver.Circom (CircomProgram(..))
import ZK.Factors

main :: IO ()
main = do
  let r1cs = r1csToCircomR1CS $ toR1CS (cpVars factors) (cpCircuit factors)
  encodeFile "trusted-setup/circuit.r1cs" r1cs
  encodeFile "trusted-setup/circuit.bin" factors