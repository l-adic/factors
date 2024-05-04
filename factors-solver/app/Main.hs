module Main where

import Circuit.Solver.Circom qualified as Circom
import Data.IORef (IORef, newIORef)
import Protolude
import System.IO.Unsafe (unsafePerformIO)
import ZK.Factors

main :: IO ()
main = mempty

stateRef :: IORef (Circom.ProgramState Fr)
stateRef = unsafePerformIO $ do
  st <- Circom.mkProgramState env
  newIORef st
{-# NOINLINE stateRef #-}

env :: Circom.ProgramEnv Fr
env = 
  Circom.mkProgramEnv (factorsVars @Fr factors) (factorsCircuit factors)

foreign export ccall init :: Int -> IO ()

init :: Int -> IO ()
init = Circom._init

foreign export ccall getNVars :: Int

getNVars :: Int
getNVars = Circom._getNVars env

foreign export ccall getVersion :: Int

getVersion :: Int
getVersion = Circom._getVersion env

foreign export ccall getRawPrime :: IO ()

getRawPrime :: IO ()
getRawPrime = Circom._getRawPrime env stateRef

foreign export ccall writeSharedRWMemory :: Int -> Word32 -> IO ()

writeSharedRWMemory :: Int -> Word32 -> IO ()
writeSharedRWMemory = Circom._writeSharedRWMemory stateRef

foreign export ccall readSharedRWMemory :: Int -> IO Word32

readSharedRWMemory :: Int -> IO Word32
readSharedRWMemory = Circom._readSharedRWMemory stateRef

foreign export ccall getFieldNumLen32 :: Int

getFieldNumLen32 :: Int
getFieldNumLen32 = Circom._getFieldNumLen32 env

foreign export ccall setInputSignal :: Word32 -> Word32 -> Int -> IO ()

setInputSignal :: Word32 -> Word32 -> Int -> IO ()
setInputSignal = Circom._setInputSignal env stateRef

foreign export ccall getInputSize :: Int

getInputSize :: Int
getInputSize = Circom._getInputSize env

foreign export ccall getInputSignalSize :: Word32 -> Word32 -> IO Int

getInputSignalSize :: Word32 -> Word32 -> IO Int
getInputSignalSize = Circom._getInputSignalSize

foreign export ccall getWitnessSize :: Int

getWitnessSize :: Int
getWitnessSize = Circom._getWitnessSize env

foreign export ccall getWitness :: Int -> IO ()

getWitness :: Int -> IO ()
getWitness = Circom._getWitness env stateRef
