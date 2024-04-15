module ZK.Factors.Circuit
  ( FactorsCircuit (..),
    factorsCircuit,
  )
where

import Circuit
import Data.Field.Galois (GaloisField)
import Data.Set qualified as Set
import Protolude

factors :: (GaloisField f) => ExprM f Wire
factors = do
  n <- deref <$> freshPublicInput
  a <- deref <$> freshPrivateInput
  b <- deref <$> freshPrivateInput
  let isFactorization = eq n (a `mul` b)
  ret $ cond isFactorization (c 1) (c 0)

data FactorsCircuit f
  = FactorsCircuit
  { fcCircuit :: ArithCircuit f,
    fcPublicInput :: Int,
    fcPrivateInputs :: [Int],
    fcOutput :: Int,
    fcNumVars :: Int
  }

factorsCircuit :: (GaloisField f) => FactorsCircuit f
factorsCircuit =
  let BuilderState {..} = snd $ runCircuitBuilder factors
   in FactorsCircuit
        { fcCircuit = bsCircuit,
          fcPublicInput = fromMaybe (panic "must have public input") $ head bsPublicInputs,
          fcPrivateInputs = bsPrivateInputs,
          fcOutput = fromMaybe (panic "must have output") $ head bsOutputs,
          fcNumVars = Set.size $ collectVariables bsCircuit
        }
