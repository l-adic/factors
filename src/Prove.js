import { WASI } from "@bjorn3/browser_wasi_shim/dist";
import { groth16, wtns } from "snarkjs";


async function _fullProve(input) {
  const wasmFile = "circuit.wasm";
  const zkeyFile = "circuit_final.zkey";
  console.log(wasmFile);
  console.log(zkeyFile);
  const wasi = new WASI([], [], []);
  let options = { additionalImports: { wasi_snapshot_preview1: wasi.wasiImport }};
  console.log(options)
  const w= {
        type: "mem"
    };
  await wtns.calculate(input, wasmFile, w, options);
  console.log("wtns calculated");
  const {proof, publicSignals} = await groth16.prove(zkeyFile, w);
  console.log("proof", proof);
  console.log("publicSignals", publicSignals);
  return {proof, publicSignals};
}

export const fullProveImpl = input => () => _fullProve(input);