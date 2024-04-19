module Deploy.ContractConfig
  ( verifierCfg
  ) where

import Chanterelle.Types.Deploy (ContractConfig, NoArgs, constructorNoArgs, noArgs)

verifierCfg :: ContractConfig NoArgs
verifierCfg =
  { filepath: "build/contracts/Groth16Verifier.json"
  , name: "Groth16Verifier"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }