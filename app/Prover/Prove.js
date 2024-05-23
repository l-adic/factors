import { WASI, PreopenDirectory, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim/dist";
import { groth16, wtns } from "snarkjs";

async function load_external_file(path) {
  const response = await fetch(path);
  const buffer = await response.arrayBuffer();
  return new File(buffer);
}

async function _fullProve(input) {
  const wasmFile = "circuit.wasm";
  const zkeyFile = "circuit_final.zkey";
  let fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),
    ConsoleStdout.lineBuffered(msg => console.warn(`[WASI stderr] ${msg}`)),
    new PreopenDirectory("/", [
      ["circuit.bin", await load_external_file("circuit.bin")]
    ])
  ];
  const wasi = new WASI([], [], fds, { debug: true });
  let options = {
    initializeWasiReactorModuleInstance: (instance) => {
      wasi.initialize(instance);
    },
    additionalWASMImports: {
      wasi_snapshot_preview1: wasi.wasiImport
    }
  };
  const w = {
        type: "mem"
    };
  await wtns.calculate(input, wasmFile, w, options);
  const {proof, publicSignals} = await groth16.prove(zkeyFile, w);
  return {proof, inputs: publicSignals};
}

export const fullProveImpl = input => () => _fullProve(input);