module ZK.Factors
  ( factors,
  )
where

import Circuit
import Circuit.Language
import Data.Field.Galois (GaloisField)
import Protolude

factors ::
  (GaloisField f) =>
  (Hashable f) =>
  ExprM f (Var Wire f 'TBool)
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
