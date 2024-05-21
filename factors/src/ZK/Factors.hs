{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZK.Factors
  ( factors,
    Fr,
  )
where

import Circuit
import Circuit.Language
import Circuit.Solver.Circom (CircomProgram, mkCircomProgram)
import Data.Field.Galois (GaloisField, Prime)
import Protolude

type Fr = Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617

factorsE :: (GaloisField f, Hashable f) => ExprM f (Var Wire f 'TBool)
factorsE = do
  n <- var_ <$> fieldInput Public "n"
  a <- var_ <$> fieldInput Private "a"
  b <- var_ <$> fieldInput Private "b"
  boolOutput "out" $ eq_ n (a * b)

factors :: CircomProgram Fr
factors =
  let BuilderState {..} = snd $ runCircuitBuilder factorsE
   in mkCircomProgram bsVars bsCircuit
