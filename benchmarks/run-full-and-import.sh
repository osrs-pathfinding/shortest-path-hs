#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
run_prefix=${RUN_PREFIX:-full}
notes=${BENCHMARK_NOTES:-full Tile A* baseline}
run="$run_prefix-$(date -u +%Y%m%dT%H%M%SZ)-$(git -C "$root" rev-parse --short HEAD)"
output="$root/out/$run/tile-astar.jsonl"
oracle="$root/benchmarks/corpus/oracle-v1.json"
clickhouse_url=${CLICKHOUSE_URL:-http://127.0.0.1:8123}
testbed=${TESTBED:-cedric}

mkdir -p "$(dirname "$output")"
cd "$root"

curl -sS --fail "${clickhouse_url%/}/ping" >/dev/null
nix-shell --run "cabal run route-bench -- --tier full --runs 3 --oracle '$oracle' --output '$output'"

nix-shell --run \
  "cabal run bench-import -- --run-id '$run' --testbed '$testbed' --notes '$notes' --clickhouse-url '$clickhouse_url' '$output'"

echo "JSONL: $output"
