#!/usr/bin/env bash
set -e
WDIR="$(mktemp -d)"
trap 'rm -rf -- "$WDIR"' EXIT

wasm32-wasi-cabal build exe:factors-solver --minimize-conflict-set
wasm32-wasi-cabal list-bin exe:factors-solver
FACTORS_WASM="$(wasm32-wasi-cabal list-bin exe:factors-solver)"
wizer \
    --allow-wasi --wasm-bulk-memory true \
    "$FACTORS_WASM" -o "$WDIR/factors-init.wasm" \
#    --mapdir /::./extract-hackage-info
if [ $# -eq 0 ]; then
    FACTORS_WASM_OPT="$WDIR/factors-init.wasm"
else
    FACTORS_WASM_OPT="$WDIR/factors-opt.wasm"
    wasm-opt "$@" "$WDIR/factors-init.wasm" -o "$FACTORS_WASM_OPT"
fi
cp "$FACTORS_WASM_OPT" ../factors-output/factors.wasm
