#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
corpus_root=${SHORTEST_PATH_CORPUS_DIR:-$root/../shortest-path-corpus}
corpus=${1:-"$corpus_root/corpus/routes-v1.json"}
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

cp "$corrected" "$corpus"
echo "validating corrected corpus"
node "$corpus_root/tools/validate.js"
echo "updated $corpus"
