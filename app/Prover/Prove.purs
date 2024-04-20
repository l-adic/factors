module Prover.Prove (fullProve) where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Promise (Promise, toAffE)
import Data.Either (either)
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Foreign (Foreign)
import Foreign.Index (readProp) as F
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Prover.Types (Inputs, Proof, decodeInputs, decodeProof)
import Record as Record
import Type.Proxy (Proxy(..))

foreign import fullProveImpl :: { a :: BigInt, b :: BigInt, n :: BigInt, out :: BigInt } -> Effect (Promise Foreign)

fullProve :: { a :: BigInt, b :: BigInt, n :: BigInt } -> Aff { proof :: Proof, inputs :: Inputs }
fullProve input = do
  p <- toAffE $ fullProveImpl $ Record.insert (Proxy :: Proxy "out") (BigInt.fromInt 1) input
  either (throwError <<< error <<< show) pure $ un Identity $ runExceptT do
    proof <- F.readProp "proof" p >>= decodeProof
    inputs <- F.readProp "inputs" p >>= decodeInputs
    pure { proof, inputs }

