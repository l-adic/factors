module Prover.Verify (verify, verifierAddress) where

import Prelude

import Contracts.Groth16Verifier as Groth16
import Data.Array (unsafeIndex)
import Data.Either (Either, hush)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (BigNumber(..))
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Web3 (Address, CallError, ChainCursor(..), UIntN, Vector, Web3, _to, defaultTransactionOptions, mkAddress, nilVector, uIntNFromBigNumber, vCons)
import Partial.Unsafe (unsafePartial)
import Prover.Types (Fp(..), Fp2(..), G1(..), G2(..), Inputs(..), Proof(..))
import Type.Proxy (Proxy(..))

fp2ForEth :: Fp2 -> Vector 2 (UIntN 256)
fp2ForEth (Fp2 { real, imag }) =
  vCons (toUInt imag) $ vCons (toUInt real) nilVector

verify
  :: forall r
   . { proof :: Proof
     , inputs :: Inputs
     | r
     }
  -> Web3 (Either CallError Boolean)
verify { proof: Proof { a: G1 a, b: G2 b, c: G1 c }, inputs: Inputs inputs } =
  let
    txOpts =
      defaultTransactionOptions
        # _to ?~ verifierAddress
  in
    Groth16.verifyProof txOpts Latest proofForContract
  where
  proofForContract =
    { _pA: vCons (toUInt a.x) $ vCons (toUInt a.y) nilVector
    , _pB: vCons (fp2ForEth b.x) $ vCons (fp2ForEth b.y) nilVector
    , _pC: vCons (toUInt c.x) $ vCons (toUInt c.y) nilVector
    , _pubSignals: unsafePartial
        $ map toUInt
        $ vCons (inputs `unsafeIndex` 0)
        $ vCons (inputs `unsafeIndex` 1) nilVector
    }

foreign import verifierAddressStr :: String

verifierAddress :: Address
verifierAddress = unsafePartial $ fromJust do
  hx <- hush $ Hex.fromString verifierAddressStr
  mkAddress hx

toUInt :: Fp -> UIntN 256
toUInt (Fp x) = unsafePartial $ fromJust $ uIntNFromBigNumber (Proxy @256) $ BigNumber x