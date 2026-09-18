#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

profile_dir="${1:-/tmp/tile-forward-profile}"
mkdir -p "$profile_dir"

cat >"$profile_dir/route.json" <<'JSON'
[
  {
    "id": "gps-natural-0012",
    "name": "Cauldron of Thunder → Ice Queen's Lair",
    "category": "gps-natural",
    "start": [2895, 9833, 0],
    "target": [2861, 9947, 0],
    "allowTransports": true,
    "tiers": ["full"],
    "distanceTag": "regional",
    "planeTag": "0-to-0"
  }
]
JSON

# --rerun-failures uses correct:false as a route/profile selector.
cat >"$profile_dir/maxed.jsonl" <<'JSONL'
{"routeId":"gps-natural-0012","accountProfile":"maxed","correct":false}
JSONL

nix-shell --run \
  'cabal build route-bench --enable-profiling --ghc-options="-fprof-late"'

exe="$(
  nix-shell --run \
    'cabal list-bin route-bench --enable-profiling --ghc-options="-fprof-late"' |
    tail -n1
)"

SPM_TILE_REVERSE_IMPL=manhattan "$exe" \
  --corpus "$profile_dir/route.json" \
  --corpus-dir "${SHORTEST_PATH_CORPUS_DIR:-../shortest-path-corpus}" \
  --oracle "${SHORTEST_PATH_CORPUS_DIR:-../shortest-path-corpus}/oracle/oracle-v1.json" \
  --output "$profile_dir/result.jsonl" \
  --rerun-failures "$profile_dir/maxed.jsonl" \
  --tier full \
  --runs 1 \
  +RTS -N1 -pj "-po${profile_dir}/forward-search.prof" -RTS

echo "JSON cost-centre profile: $profile_dir/forward-search.prof"
echo "Benchmark result:         $profile_dir/result.jsonl"
