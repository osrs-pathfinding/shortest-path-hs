#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
corpus_dir=${SHORTEST_PATH_CORPUS_DIR:-"$root/../shortest-path-corpus"}
corpus=${CORPUS:-"$corpus_dir/corpus/routes-v1.json"}
tier=${TIER:-full}
jobs=${JOBS:-4}
runs=${RUNS:-1}
output=${OUTPUT_DIR:-"$root/out/corpus-check-$(date +%Y%m%d-%H%M%S)"}
oracle="$output/oracle.json"
tileOutput="$output/tile-astar.jsonl"
dijkstraLog="$output/dijkstra.log"
audit="$output/account-vars.txt"

mkdir -p "$output"
cd "$root"
node "$corpus_dir/tools/validate.js"

echo "[1/3] Reference Dijkstra oracle/benchmark"
nix-shell --run \
  "cabal run route-bench -- --corpus-dir '$corpus_dir' --corpus '$corpus' --oracle '$oracle' --tier '$tier' --jobs '$jobs' --write-oracle" \
  2>&1 | tee "$dijkstraLog"

echo "[2/3] Tile A* correctness and diagnostic comparison"
nix-shell --run \
  "cabal run route-bench -- --corpus-dir '$corpus_dir' --corpus '$corpus' --oracle '$oracle' --output '$tileOutput' --tier '$tier' --runs '$runs' --jobs '$jobs'" \
  2>&1 | tee "$output/tile-astar.log"

if rg -q '"correct"\s*:\s*false' "$tileOutput"; then
  echo "Tile A* correctness failures found in $tileOutput" >&2
  exit 1
fi

echo "[3/3] Account varbit/varplayer modelling audit"
nix-shell --run 'cabal run account-profile -- vars' 2>&1 | tee "$audit"

echo
echo "Reports written to $output"
echo "Unmodelled entries:"
rg -n 'UNMODELLED|UNAVAILABLE' "$audit" || true
