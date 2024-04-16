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
  n <- deref <$> freshPublicInput "n"
  a <- deref <$> freshPrivateInput "a"
  b <- deref <$> freshPrivateInput "b"
  let isFactorization = eq n (a `mul` b)
  ret $ cond isFactorization (c 1) (c 0)

data FactorsCircuit f
  = FactorsCircuit
  { fcCircuit :: ArithCircuit f,
    fcPublicInput :: Int,
    fcPrivateInputs :: (Int, Int),
    fcOutput :: Int,
    fcNumVars :: Int,
    fcVars :: CircuitVars Text
  }

factorsCircuit :: (GaloisField f) => FactorsCircuit f
factorsCircuit =
  let BuilderState {..} = snd $ runCircuitBuilder factors
   in FactorsCircuit
        { fcCircuit = bsCircuit,
          fcPublicInput = fromMaybe (panic "must have public input") (head . Set.toList . cvPublicInputs $ bsVars),
          fcPrivateInputs = fromMaybe (panic "must have private inputs") $
            let privInputs = Set.toList $ cvPrivateInputs bsVars
            in case privInputs of
              [i1, i2] -> Just (i1, i2)
              _ -> Nothing,
          fcOutput = fromMaybe (panic "must have output") (head . Set.toList . cvOutputs $ bsVars),
          fcNumVars = Set.size . cvVars $ bsVars,
          fcVars = bsVars
        }
