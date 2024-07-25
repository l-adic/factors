import { WASI, PreopenDirectory, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim/dist";
import { groth16 } from "snarkjs";

import * as fastFile from "fastfile";
import { WitnessCalculatorBuilder } from "circom_runtime";

async function _fullProve(input) {
  const zkeyFile = "circuit_final.zkey";
  let instance = await createWASMInstance();
  const w = {
    type: "mem"
  };
  await wtnsCalculate(input, instance, w);
  const { proof, publicSignals } = await groth16.prove(zkeyFile, w);
  return { proof, inputs: publicSignals };
}

export const fullProveImpl = input => () => _fullProve(input);

async function createWASMInstance() {
  let memorySize = 32767;
  let memory;
  let memoryAllocated = false;
  while (!memoryAllocated) {
    try {
      memory = new WebAssembly.Memory({ initial: memorySize });
      memoryAllocated = true;
    } catch (err) {
      if (memorySize === 1) {
        throw err;
      }
      console.warn("Could not allocate " + memorySize * 1024 * 64 + " bytes. This may cause severe instability. Trying with " + memorySize * 1024 * 64 / 2 + " bytes");
      memorySize = Math.floor(memorySize / 2);
    }
  }

  const fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),
    ConsoleStdout.lineBuffered(msg => console.warn(`[WASI stderr] ${msg}`)),
    new PreopenDirectory("/", [
      ["circuit.bin", await load_external_file("circuit.bin")]
    ])
  ];

  const wasi = new WASI([], [], fds, { debug: true });

  const fdWasm = await fastFile.readExisting("circuit.wasm");
  const code = await fdWasm.read(fdWasm.totalSize);
  await fdWasm.close();

  const wasmModule = await WebAssembly.compile(code);

  const instance = await WebAssembly.instantiate(wasmModule, { wasi_snapshot_preview1: wasi.wasiImport });

  wasi.initialize(instance);

  return instance;

};

async function wtnsCalculate(input, wasm, wtnsFileName) {

  const wc = await WitnessCalculatorBuilder(wasm);
  const fdWtns = await fastFile.createOverride(wtnsFileName);

  const w = await wc.calculateWTNSBin(input);

  await fdWtns.write(w);
  await fdWtns.close();
};

async function load_external_file(path) {
  const response = await fetch(path);
  const buffer = await response.arrayBuffer();
  return new File(buffer);
};