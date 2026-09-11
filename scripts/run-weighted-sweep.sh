#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"

if [[ -z ${IN_NIX_SHELL:-} ]]; then
  exec nix-shell --run 'bash scripts/run-weighted-sweep.sh'
fi

weights=(1.0 1.1 1.25 1.5 2.0 3.0)
commit=$(git rev-parse --short HEAD)
timestamp=$(date -u +%Y%m%dT%H%M%SZ)
sweep="weighted-astar-standard-${timestamp}-${commit}"
output_dir="$root/out/$sweep"
corpus="$root/benchmarks/corpus/routes-v1.json"
oracle="$root/benchmarks/corpus/oracle-v1.json"
clickhouse_url=${CLICKHOUSE_URL:-http://127.0.0.1:8123}
testbed=${TESTBED:-cedric}

mkdir -p "$output_dir"
curl -sS --fail "${clickhouse_url%/}/ping" >/dev/null
cabal build route-bench bench-import
route_bench=$(cabal list-bin route-bench)
bench_import=$(cabal list-bin bench-import)

run_ids=()
outputs=()
for weight in "${weights[@]}"; do
  weight_id=${weight/./p}
  run_id="${sweep}-w${weight_id}"
  output="$output_dir/w${weight}.jsonl"

  "$route_bench" \
    --tier standard \
    --runs 3 \
    --corpus "$corpus" \
    --oracle "$oracle" \
    --heuristic-weight "$weight" \
    --output "$output"

  "$bench_import" \
    --run-id "$run_id" \
    --sweep "$sweep" \
    --testbed "$testbed" \
    --notes "weighted A* standard sweep, weight $weight" \
    --clickhouse-url "$clickhouse_url" \
    --corpus "$corpus" \
    "$output"

  if rg -q '"correct"[[:space:]]*:[[:space:]]*false' "$output"; then
    echo "WARNING: correctness failures recorded in $run_id" >&2
  fi

  run_ids+=("$run_id")
  outputs+=("$output")
done

echo
echo "Sweep: $sweep"
for i in "${!run_ids[@]}"; do
  echo "${run_ids[$i]}  ${outputs[$i]}"
done
