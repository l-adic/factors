module ZK.Factors.Witness where

import Circuit
import Data.Field.Galois (PrimeField)
import Data.Map qualified as Map
import Data.Propagator (PropagatedNum)
import Protolude
import R1CS (Witness (..))
import ZK.Factors.Circuit (FactorsCircuit (..), factorsCircuit)

solver ::
  (PrimeField f, PropagatedNum f) =>
  -- | public input n
  f ->
  -- | private input (a,b)
  (f, f) ->
  Witness f
solver n (a, b) =
  let FactorsCircuit {..} = factorsCircuit
      publicInputs = Map.singleton fcPublicInput n
      privateInputs = Map.fromList 
        [ (fst fcPrivateInputs, a)
        , (snd fcPrivateInputs, b)
        ]
      inputs = publicInputs <> privateInputs
      w = solve inputs fcCircuit
   in Witness w
