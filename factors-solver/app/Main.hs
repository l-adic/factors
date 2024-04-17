module Main where

import Circuit.Solver.Circom qualified as Circom
import Data.IORef (IORef, newIORef)
import Protolude
import System.IO.Unsafe (unsafePerformIO)
import ZK.Factors

main :: IO ()
main = mempty

stateRef :: IORef (Circom.ProgramState Fr)
stateRef =
  unsafePerformIO $
    newIORef $
      Circom.mkProgramState (factorsVars $ factors @Fr)
{-# NOINLINE stateRef #-}

env :: Circom.ProgramEnv Fr
env =
  unsafePerformIO $
    Circom.mkProgramEnv (factorsCircuit factors)
{-# NOINLINE env #-}

foreign export ccall init :: Int -> IO ()

init :: Int -> IO ()
init = Circom._init

foreign export ccall getNVars :: IO Int

getNVars :: IO Int
getNVars = Circom._getNVars stateRef

foreign export ccall getVersion :: Int

getVersion :: Int
getVersion = Circom._getVersion env

foreign export ccall getRawPrime :: IO ()

getRawPrime :: IO ()
getRawPrime = Circom._getRawPrime env

foreign export ccall writeSharedRWMemory :: Int -> Word32 -> IO ()

writeSharedRWMemory :: Int -> Word32 -> IO ()
writeSharedRWMemory = Circom._writeSharedRWMemory env

foreign export ccall readSharedRWMemory :: Int -> IO Word32

readSharedRWMemory :: Int -> IO Word32
readSharedRWMemory = Circom._readSharedRWMemory env

foreign export ccall getFieldNumLen32 :: Int

getFieldNumLen32 :: Int
getFieldNumLen32 = Circom._getFieldNumLen32 env

foreign export ccall setInputSignal :: Word32 -> Word32 -> Int -> IO ()

setInputSignal :: Word32 -> Word32 -> Int -> IO ()
setInputSignal = Circom._setInputSignal env stateRef

foreign export ccall getWitnessSize :: IO Int

getWitnessSize :: IO Int
getWitnessSize = Circom._getWitnessSize stateRef

foreign export ccall getWitness :: Int -> IO ()

getWitness :: Int -> IO ()
getWitness = Circom._getWitness env stateRef