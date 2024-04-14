module Main where

import Data.Binary (encode)
import Data.Binary.Get (runGet)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BU
import Data.Map qualified as Map
import Data.String (String)
import Foreign hiding (void)
import Foreign.C.Types
import Protolude
import R1CS (Inputs (..), Witness)
import R1CS.Circom (FieldSize (..), getInputs, witnessToCircomWitness)
import ZK.Factors (FactorsCircuit (..), Fr, factorsCircuit, solver)

main :: IO ()
main = mempty

-- marshalling

foreign export ccall mallocPtr :: IO (Ptr (Ptr a))

mallocPtr :: IO (Ptr (Ptr a))
mallocPtr = malloc

foreign export ccall calculateWitnessRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

calculateWitnessRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
calculateWitnessRaw inputPtr inputLen outputPtrPtr = do
  inputs <-
    runGet (getInputs (FieldSize 32) nInputs) . BL.fromStrict <$> BU.unsafePackMallocCStringLen (inputPtr, inputLen)
  let outputBytes =
        BL.toStrict $ encode $ witnessToCircomWitness $ calculateWitness inputs
  BU.unsafeUseAsCStringLen outputBytes \(buf, len) -> do
    putStrLn ("Holy shit I'm printing this from inside a haskell program compiled to wasm" :: String)
    outputPtr <- mallocBytes len
    poke outputPtrPtr outputPtr
    copyBytes outputPtr buf len
    pure len
  where
    -- the factors program has 1 public input and 2 private inputs
    nInputs :: Int
    nInputs = 3

calculateWitness :: Inputs Fr -> Witness Fr
calculateWitness (Inputs inputs) =
  let FactorsCircuit {..} = factorsCircuit @Fr
      n =
        fromMaybe (panic "no private input") $
          Map.lookup fcPublicInput inputs
      (a, b) = fromMaybe (panic "no public inputs") $ do
        case fcPrivateInputs of
          [i1, i2] -> (,) <$> Map.lookup i1 inputs <*> Map.lookup i2 inputs
          _ -> Nothing
   in solver n (a, b)
