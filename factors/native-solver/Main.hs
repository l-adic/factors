{-# LANGUAGE TemplateHaskell #-}

module Main where

import Circuit.Solver.Circom (CircomProgram, nativeGenWitness)
import Data.Aeson (decodeFileStrict)
import Data.Binary (decode, encodeFile)
import Data.FileEmbed (embedFileRelative)
import Data.String.Conversions (cs)
import Protolude
import ZK.Factors (Fr)

main :: IO ()
main = do
  mInputs <- decodeFileStrict "trusted-setup/inputs.json"
  inputs <- maybe (panic "Failed to decode inputs") (pure . map fromInteger) mInputs
  let circuit :: CircomProgram Fr
      circuit = decode $ cs circuitBin
      wtns = nativeGenWitness circuit inputs
  encodeFile "trusted-setup/circuit.wtns" wtns

-- We do this to avoid compiling the program twice. It's easier to read it
-- from a file after initial compilation.
circuitBin :: ByteString
circuitBin = $(embedFileRelative "../trusted-setup/circuit.bin")
