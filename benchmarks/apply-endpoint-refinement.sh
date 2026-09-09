#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
corpus=${1:-"$root/benchmarks/corpus/routes-v1.json"}
temporary=$(mktemp -d "${TMPDIR:-/tmp}/endpoint-refinement.XXXXXX")
trap 'rm -rf "$temporary"' EXIT

facts="$temporary/world-facts.duckdb"
corrected="$temporary/routes-v1.json"

cd "$root"
echo "building world facts"
nix-shell --run "cabal run world-facts -- --output '$facts'"

echo "refining existing endpoints"
WORLD_FACTS_DB="$facts" nix-shell --run \
  "node benchmarks/refine-endpoints.js '$corpus' '$corrected'"

echo "validating corrected corpus"
WORLD_FACTS_DB="$facts" nix-shell --run \
  "node benchmarks/validate-corpus.js '$corrected' --world-facts '$facts'"

cp "$corrected" "$corpus"
echo "updated $corpus"
