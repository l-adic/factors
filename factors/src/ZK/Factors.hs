{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZK.Factors
  ( Factors (..),
    factors,
    Fr,
  )
where

import Circuit
import Data.Field.Galois (GaloisField, Prime)
import Protolude

factorsE :: (GaloisField f) => ExprM f Wire
factorsE = do
  n <- deref <$> freshPublicInput "n"
  a <- deref <$> freshPrivateInput "a"
  b <- deref <$> freshPrivateInput "b"
  let isFactorization = eq n (a `mul` b)
  ret $ cond isFactorization (c 1) (c 0)

type Fr = Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617

data Factors f = Factors
  { factorsCircuit :: ArithCircuit f,
    factorsVars :: CircuitVars Text
  }

factors :: forall f. (GaloisField f) => Factors f
factors =
  let BuilderState {..} = snd $ runCircuitBuilder (factorsE @f)
   in Factors
        { factorsCircuit = bsCircuit,
          factorsVars = bsVars
        }
