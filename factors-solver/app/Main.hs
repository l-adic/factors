module Main where

import Data.Field.Galois (char, fromP)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Vector qualified as V
import Data.Vector.Mutable (IOVector)
import Data.Vector.Mutable qualified as MV
import Foreign hiding (void)
import Protolude
import R1CS (Inputs (..), Witness (..))
import R1CS.Circom (FieldSize (..), integerFromLittleEndian, integerToLittleEndian, n32)
import System.IO.Unsafe (unsafePerformIO)
import ZK.Factors (FactorsCircuit (..), Fr, factorsCircuit, solver)

main :: IO ()
main = mempty

-- marshalling

-- foreign export ccall mallocPtr :: IO (Ptr (Ptr a))
--
-- mallocPtr :: IO (Ptr (Ptr a))
-- mallocPtr = malloc
--
-- foreign export ccall calculateWitnessRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
--
-- calculateWitnessRaw :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
-- calculateWitnessRaw inputPtr inputLen outputPtrPtr = do
--  inputs <-
--    runGet (getInputs (FieldSize 32) nInputs) . BL.fromStrict <$> BU.unsafePackMallocCStringLen (inputPtr, inputLen)
--  let outputBytes =
--        BL.toStrict $ encode $ witnessToCircomWitness $ calculateWitness inputs
--  BU.unsafeUseAsCStringLen outputBytes \(buf, len) -> do
--    putStrLn ("Holy shit I'm printing this from inside a haskell program compiled to wasm" :: String)
--    outputPtr <- mallocBytes len
--    poke outputPtrPtr outputPtr
--    copyBytes outputPtr buf len
--    pure len
--  where
--    -- the factors program has 1 public input and 2 private inputs
--    nInputs :: Int
--    nInputs = 3

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

{-
  init

  getNVars :: Int
  getVersion :: Int, returns 1
  getRawPrime -- sets Integer in shared buffer
  getFieldNumLen32 -- how many chunks of Word32 do the integers come in ? they are 32 bytes = 256 bits. 256/32 = 8 (FieldSize in bits / 8) = FieldSize * 8 / 32
  readSharedRWMemory :: IO (Word32)
  writeSharedRWMemory :: Int -> Word32 -> IO ()
  readSharedRWMemory :: Int -> IO (Word32)

  -- assumes that the signal value is in the r/w buffer
  setIntputSignal
    :: Word32 -> -- msb
       Word32 -> --lsb
       Int -> -- index
       IO ()

-}

data ProgramState f
  = ProgramState
  { psSharedRWMemory :: IOVector Word32,
    psFieldSize :: FieldSize,
    psRawPrime :: Integer,
    psInputsSize :: Int,
    psInputs :: IORef (Inputs f),
    psWitnessSize :: Int,
    psWitness :: IORef (Witness f)
  }

stateRef :: IORef (Maybe (ProgramState Fr))
stateRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE stateRef #-}

foreign export ccall init :: IO ()

init :: IO ()
init = do
  let fieldSize = FieldSize 32
  sharedRWMemory <- MV.replicate (n32 fieldSize) 0
  let inputsSize = 3
  inputs <- newIORef (Inputs mempty)
  let witnessSize = fcNumVars $ factorsCircuit @Fr
  wtns <- newIORef (Witness mempty)
  let st =
        ProgramState
          { psSharedRWMemory = sharedRWMemory,
            psFieldSize = fieldSize,
            psRawPrime = toInteger $ char (1 :: Fr),
            psInputsSize = inputsSize,
            psInputs = inputs,
            psWitnessSize = witnessSize,
            psWitness = wtns
          }
  writeIORef stateRef (Just st)

foreign export ccall getNVars :: IO Int

getNVars :: IO Int
getNVars = onlyWhenInitialized $ \st ->
  pure $ psWitnessSize st

foreign export ccall getVersion :: IO Int

getVersion :: IO Int
getVersion = pure 1

foreign export ccall getRawPrime :: IO ()

getRawPrime :: IO ()
getRawPrime = onlyWhenInitialized $ \ProgramState {psFieldSize, psRawPrime} ->
  let chunks = integerToLittleEndian psFieldSize psRawPrime
   in forM_ [0 .. n32 psFieldSize] \i -> writeSharedRWMemory i (chunks V.! i)

foreign export ccall writeSharedRWMemory :: Int -> Word32 -> IO ()

writeSharedRWMemory :: Int -> Word32 -> IO ()
writeSharedRWMemory i v = onlyWhenInitialized $ \st ->
  MV.write (psSharedRWMemory st) i v

foreign export ccall readSharedRWMemory :: Int -> IO Word32

readSharedRWMemory :: Int -> IO Word32
readSharedRWMemory i = onlyWhenInitialized $ \st ->
  MV.read (psSharedRWMemory st) i

foreign export ccall getFieldNumLen32 :: IO Int

getFieldNumLen32 :: IO Int
getFieldNumLen32 = onlyWhenInitialized $ \st ->
  pure $ n32 $ psFieldSize st

foreign export ccall setInputSignal :: Word32 -> Word32 -> Int -> IO ()

setInputSignal :: Word32 -> Word32 -> Int -> IO ()
setInputSignal _ _ i = onlyWhenInitialized $ \st -> do
  Inputs inputs <- readIORef $ psInputs st
  v <- V.generateM (n32 $ psFieldSize st) \j ->
    MV.read (psSharedRWMemory st) j
  let input = fromInteger @Fr . integerFromLittleEndian $ v
  writeIORef (psInputs st) $ Inputs $ Map.insert i input inputs
  when (Map.size inputs == psInputsSize st) $ do
    let wtns = calculateWitness $ Inputs inputs
    writeIORef (psWitness st) wtns

foreign export ccall getWitnessSize :: IO Int

getWitnessSize :: IO Int
getWitnessSize = onlyWhenInitialized $ \st ->
  pure $ psWitnessSize st

foreign export ccall getWitness :: Int -> IO ()

getWitness :: Int -> IO ()
getWitness i = onlyWhenInitialized $ \st -> do
  Witness wtns <- readIORef $ psWitness st
  let wtn = maybe (panic $ "missing witness " <> show i) fromP $ Map.lookup i wtns
  let chunks = integerToLittleEndian (psFieldSize st) $ fromInteger wtn
  forM_ [0 .. n32 (psFieldSize st)] \j ->
    writeSharedRWMemory j (chunks V.! j)

--------------------------------------------------------------------------------

onlyWhenInitialized :: (ProgramState Fr -> IO a) -> IO a
onlyWhenInitialized action = do
  st <- readIORef stateRef
  case st of
    Just s -> action s
    Nothing -> panic "state not initialized"