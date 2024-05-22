import { WASI, PreopenDirectory, File, strace } from "@bjorn3/browser_wasi_shim/dist";
import { groth16, wtns } from "snarkjs";

async function load_external_file(path) {
  const response = await fetch(path);
  const buffer = await response.arrayBuffer();
  console.log(buffer);
  return new File(buffer);
}

async function _fullProve(input) {
  const wasmFile = "circuit.wasm";
  const zkeyFile = "circuit_final.zkey";
  let fds = [
    new PreopenDirectory("/", [
      ["circuit.bin", await load_external_file("circuit.bin")]
    ])
  ];
  console.log(fds);
  const wasi = new WASI([], [], fds, { debug: true });
  console.log("wasi pre-initialized");
  console.log(wasi);
  let options = {
    initializeWasi: (instance) => {
      wasi.initialize(instance);
      console.log("wasi post-initialized");
      console.log(wasi);
    },
    additionalWASMImports: {
      wasi_snapshot_preview1: strace(wasi.wasiImport, ["proc_exit"])
    }
  };

  const w = {
        type: "mem"
    };
  await wtns.calculate(input, wasmFile, w, options);
  console.log("fds");
  console.log(fds);
  const {proof, publicSignals} = await groth16.prove(zkeyFile, w);
  return {proof, inputs: publicSignals};
}

export const fullProveImpl = input => () => _fullProve(input);