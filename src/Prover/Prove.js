import { WASI } from "@bjorn3/browser_wasi_shim/dist";
import { groth16, wtns } from "snarkjs";


async function _fullProve(input) {
  const wasmFile = "circuit.wasm";
  const zkeyFile = "circuit_final.zkey";
  const wasi = new WASI([], [], []);
  let options = { additionalImports: { wasi_snapshot_preview1: wasi.wasiImport }};
  const w= {
        type: "mem"
    };
  await wtns.calculate(input, wasmFile, w, options);
  const {proof, publicSignals} = await groth16.prove(zkeyFile, w);
  return {proof, inputs: publicSignals};
}

export const fullProveImpl = input => () => _fullProve(input);