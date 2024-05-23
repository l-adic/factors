{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZK.Factors
  ( factors,
    Fr,
  )
where

import Circuit
import Circuit.Language
import Data.Field.Galois (GaloisField, Prime)
import Protolude

type Fr = Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617

factors :: (GaloisField f, Hashable f) => ExprM f (Var Wire f 'TBool)
factors = do
  a <- var_ <$> fieldInput Private "a"
  b <- var_ <$> fieldInput Private "b"
  n <- var_ <$> fieldInput Public "n"
  let cs =
        [ neq_ n a,
          neq_ n b,
          eq_ n (a * b)
        ]
  boolOutput "out" $ unAnd_ $ foldMap And_ cs
