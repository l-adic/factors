{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZK.Factors
  ( FactorsCircuit (..),
    factorsCircuit,
    solver,
    Fr,
  )
where

import Data.Field.Galois (Prime)
import Data.Propagator (Propagated, PropagatedNum)
import GHC.TypeLits (KnownNat)
import ZK.Factors.Circuit (FactorsCircuit (..), factorsCircuit)
import ZK.Factors.Witness (solver)

type Fr = Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617

instance (KnownNat p) => Propagated (Prime p)

instance (KnownNat p) => PropagatedNum (Prime p)