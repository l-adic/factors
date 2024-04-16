module Main where

import Data.Binary (encodeFile)
import Protolude
import R1CS (toR1CS)
import R1CS.Circom (r1csToCircomR1CS)
import ZK.Factors (FactorsCircuit (..), Fr, factorsCircuit)
import Text.PrettyPrint.Leijen.Text (Pretty (..), text)
import Data.String

main :: IO ()
main = do
  let fc = factorsCircuit @Fr
  let r1cs = toR1CS $ fcCircuit fc
  putStrLn @String $ show $ fcOutput fc
  encodeFile "factors-output/factors.r1cs" $ r1csToCircomR1CS r1cs
