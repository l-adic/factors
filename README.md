# Factors

The hello world app of ZK: prove you know a non-trivial factorization of an integer without revealing the factors. 

This repo serves as a guide to show how to plug a ZK Haskell program into existing proving software. In this case we want to build a browser based proving application with an Ethereum smart contract verifier, and we will make use of the [snarkjs](https://github.com/iden3/snarkjs) proving library. There are similar integrations for [arkworks](https://github.com/arkworks-rs), the [nova](https://github.com/microsoft/Nova) folding scheme and more to come.

## Setup

### nix
You must have nix installed and ideally [direnv](https://github.com/nix-community/nix-direnv). The haskell builds are managed by different nix shells depending on the directory because we require both the normal and WASM GHC.

### npm
All other builds/tools are managed via npm. To get started, run 

```
> npm install
```

### Ethereum node
You should have a local ethereum node with an unlocked default account and the web3 api running on the default port `8545`. E.g. use hardhat or [cliquebait](https://github.com/f-o-a-m/cliquebait):

```
> docker run --rm -it -p 8545:8545 foamspace/cliquebait:latest
```

## Contents

### The factors zk program
A ZK program written in a Haskell DSL that expresses a factorization of a public input `n` into a product of secret inputs `a` and `b`. The factorization must be non-trivial, i.e. `a /= 1 && b /= 1`.

You can produce a circom compatible `r1cs` file for this program by running 

```
> cabal run factors -- compile --r1cs trusted-setup/circuit.r1cs --constraints trusted-setup/circuit.bin
```

You should see the artifacts 

```
trusted-setup
├── circuit.bin
├── circuit.r1cs
└ ...
```

The `circuit.r1cs` file is the R1CS that is expected by snarkjs for proving/verification key generation. The `circuit.bin` is the binary serialization of the constraints represented by the compiled circuit. This file is required in order to evaluate the circuit, i.e. generate a witness.

### A factors program constraint solver
A constraint solver applied to the `factors` program. You can produce a circom compatible WASM binary for this solver by running

```
> cd wasm-solver
> ./build-wasm
```

You should see an artifact `www/circuit.wasm`.

### A Groth16 Solidity Verifying contract
Assuming you have run the above, you should see a `circuit.r1cs` file in the `trusted-setup` directory. To produce a final proving key and solidity verifier:

```
> npx snarkjs groth16 setup trusted-setup/circuit.r1cs trusted-setup/pot14_final.ptau trusted-setup/circuit_final.zkey
> npx snarkjs zkey export solidityverifier trusted-setup/circuit_final.zkey contracts/Groth16Verifier.sol
```

You can verify your proving key:

```
> npx snarkjs zkey verify trusted-setup/circuit.r1cs trusted-setup/pot14_final.ptau trusted-setup/circuit_final.zkey
```

NOTE: It is also possible to generate a witness, construct a proof and verify it using the standard `snarkjs` cli commands with the artifacts we have created.

You can comple the contracts, build the purescript ffi, and deploy this smart contract via 

```
> npm run chanterelle-build
> npm run chanterelle-deploy
```

### A frontend application
Assuming you have done the previous steps, copy the proving key and the compiled circuit to the `www` folder

```
> cp trusted-setup/circuit_final.zkey trusted-setup/circuit.bin www
```

You should see the `circuit.wasm` solver binary is already there. Assuming you have deployed the verifying contract, you can start the frontend:

```
export VERIFIER_ADDRESS=$(jq -r '.networks."420123".address' build/contracts/Groth16Verifier.json) && npm run parcel
```

NOTE: I used cliquebaite to write this readme, which has networkId/chainId `420123`. If you have a different chainId, you will need to subsitute it in the above command or just find the address in the artifact manually.

You should see a form load to test the application
