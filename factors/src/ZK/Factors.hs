{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZK.Factors
  ( Factors (..),
    factors,
    Fr,
  )
where

import Circuit
import Circuit.Language
import R1CS.Circom (circomReindexMap)
import Data.Field.Galois (GaloisField, Prime)
import Protolude

type Fr = Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617

factorsE :: (GaloisField f, Hashable f) => ExprM f (Var Wire f Bool)
factorsE = do
  n <- var_ <$> fieldInput Public "n"
  a <- var_ <$> fieldInput Private "a"
  b <- var_ <$> fieldInput Private "b"
  boolOutput "out" $ eq_ n (a * b)

data Factors f = Factors
  { factorsCircuit :: ArithCircuit f,
    factorsVars :: CircuitVars Text
  }

factors :: forall f. (GaloisField f, Hashable f) => Factors f
factors =
  let BuilderState {..} = snd $ runCircuitBuilder (factorsE @f)
      f = circomReindexMap bsVars
   in Factors
        { factorsCircuit = reindex f bsCircuit,
          factorsVars = reindex f bsVars
        }
