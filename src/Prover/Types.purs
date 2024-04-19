module Prover.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign (F, Foreign)
import Foreign (ForeignError(..), fail, readArray, readString, unsafeToForeign) as F
import Foreign.Index (readProp, (!)) as F
import JS.BigInt (BigInt)
import JS.BigInt as BigInt

newtype Fp = Fp BigInt

derive newtype instance Show Fp
derive newtype instance Eq Fp

decodeFp :: Foreign -> F Fp
decodeFp x = Fp <$> do
  f <- F.readString x
  case BigInt.fromString f of
    Just a -> pure a
    Nothing -> F.fail $ F.ForeignError "Failed to parse BigInt"

encodeFp :: Fp -> Foreign
encodeFp (Fp x) = F.unsafeToForeign $ BigInt.toString x

-- | An elliptic curve point in projective coordinates
newtype G1 = G1 { x :: Fp, y :: Fp, z :: Fp }

derive newtype instance Show G1
derive newtype instance Eq G1

decodeG1 :: Foreign -> F G1
decodeG1 fs = do
  x <- decodeFp =<< fs F.! 0
  y <- decodeFp =<< fs F.! 1
  z <- decodeFp =<< fs F.! 2
  pure $ G1 { x, y, z }

-- for some reason snarkjs includes a third value which is always 1
encodeG1 :: G1 -> Foreign
encodeG1 (G1 { x, y, z }) = F.unsafeToForeign $
  map encodeFp [ x, y, z ]

newtype Fp2 = Fp2 { real :: Fp, imag :: Fp }

derive newtype instance Show Fp2
derive newtype instance Eq Fp2

decodeFp2 :: Foreign -> F Fp2
decodeFp2 fs = do
  real <- decodeFp =<< fs F.! 0
  imag <- decodeFp =<< fs F.! 1
  pure $ Fp2 { real, imag }

encodeFp2 :: Fp2 -> Foreign
encodeFp2 (Fp2 { real, imag }) = F.unsafeToForeign $
  map encodeFp [ real, imag ]

newtype G2 = G2 { x :: Fp2, y :: Fp2, z :: Fp2 }

derive newtype instance Show G2
derive newtype instance Eq G2

encodeG2 :: G2 -> Foreign
encodeG2 (G2 { x, y, z }) = F.unsafeToForeign $
  map encodeFp2 [ x, y, z ]

decodeG2 :: Foreign -> F G2
decodeG2 fs = do
  x <- decodeFp2 =<< fs F.! 0
  y <- decodeFp2 =<< fs F.! 1
  z <- decodeFp2 =<< fs F.! 2
  pure $ G2 { x, y, z }

newtype Proof = Proof
  { a :: G1
  , b :: G2
  , c :: G1
  }

derive newtype instance Show Proof
derive newtype instance Eq Proof

encodeProof :: Proof -> Foreign
encodeProof (Proof { a, b, c }) =
  F.unsafeToForeign { pi_a: encodeG1 a, pi_b: encodeG2 b, pi_c: encodeG1 c }

decodeProof :: Foreign -> F Proof
decodeProof obj = do
  a <- decodeG1 =<< F.readProp "pi_a" obj
  b <- decodeG2 =<< F.readProp "pi_b" obj
  c <- decodeG1 =<< F.readProp "pi_c" obj
  pure $ Proof { a, b, c }

newtype VerifyingKey = VerifyingKey
  { alpha1 :: G1
  , beta2 :: G2
  , gamma2 :: G2
  , delta2 :: G2
  , ic :: Array G1
  }

derive newtype instance Show VerifyingKey
derive newtype instance Eq VerifyingKey

newtype Inputs = Inputs (Array Fp)

derive newtype instance Show Inputs
derive newtype instance Eq Inputs

encodeInputs :: Inputs -> Foreign
encodeInputs (Inputs xs) = F.unsafeToForeign $ encodeFp <$> xs

decodeInputs :: Foreign -> F Inputs
decodeInputs obj = do
  xs <- F.readArray obj >>= traverse decodeFp
  pure $ Inputs xs
