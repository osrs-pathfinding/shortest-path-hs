#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

output_dir="${1:-/tmp/cauldron-ice-queen-counters}"
if [[ -z "${IN_NIX_SHELL:-}" ]]; then
  printf -v quoted_output_dir %q "$output_dir"
  exec nix-shell --run "bash benchmarks/inspect-heuristic-counters.sh $quoted_output_dir"
fi

mkdir -p "$output_dir"

node - "${SHORTEST_PATH_CORPUS_DIR:-../shortest-path-corpus}/corpus/routes-v1.json" "$output_dir/route.json" <<'JS'
const fs = require("fs");
const routes = JSON.parse(fs.readFileSync(process.argv[2], "utf8"));
const route = routes.find(({id}) => id === "gps-natural-0012");
if (!route) throw new Error("gps-natural-0012 is missing from routes-v1.json");
fs.writeFileSync(process.argv[3], JSON.stringify([route]));
JS

printf '%s\n' '{"routeId":"gps-natural-0012","accountProfile":"maxed","correct":false}' > "$output_dir/maxed.jsonl"

SPM_TILE_REVERSE_IMPL=manhattan SPM_TILE_REVERSE_COUNTERS=1 \
  cabal run route-bench -- \
    --corpus "$output_dir/route.json" \
    --corpus-dir "${SHORTEST_PATH_CORPUS_DIR:-../shortest-path-corpus}" \
    --oracle "${SHORTEST_PATH_CORPUS_DIR:-../shortest-path-corpus}/oracle/oracle-v1.json" \
    --output "$output_dir/result.jsonl" \
    --rerun-failures "$output_dir/maxed.jsonl" \
    --tier full \
    --runs 1

node - "$output_dir/result.jsonl" <<'JS'
const fs = require("fs");
const rows = fs.readFileSync(process.argv[2], "utf8").trim().split("\n");
if (rows.length !== 1) throw new Error(`expected one benchmark row, got ${rows.length}`);
const result = JSON.parse(rows[0]);
const t = result.timings;
console.log(JSON.stringify({
  route_id: result.routeId,
  route_name: result.routeName,
  account_profile: result.accountProfile,
  reachable: result.reachable,
  correct: result.correct,
  path_cost: result.cost,
  expected_cost: result.expectedCost,
  total_ms: t.totalMs,
  forward_search_ms: t.forwardSearchMs,
  account_prepare_ms: t.accountPrepareMs,
  heuristic_prepare_ms: t.targetPrepareMs,
  heuristic_setup_ms: t.setupMs,
  reverse_search_ms: t.reverseDijkstraMs,
  seed_table_ms: t.seedTableMs,
  forward_allocated_bytes: t.forwardAllocatedBytes,
  forward_ns_per_state: t.forwardNsPerState,
  forward_allocated_bytes_per_state: t.forwardAllocatedBytesPerState,
  states_popped: t.statesPopped,
  unique_states_reached: t.uniqueStatesReached,
  pq_pushes: t.pqPushes,
  stale_entries: t.staleEntries,
  walking_relaxations: t.walkingRelaxations,
  transport_relaxations: t.transportRelaxations,
  heuristic_evaluations: t.heuristicEvaluations,
  unreachable_prunes: t.unreachablePrunes,
  unknown_component_prunes: t.unknownComponentPrunes,
  no_reverse_seed_prunes: t.noReverseSeedPrunes,
  best_bank_updates: t.bestBankUpdates,
  final_best_bank_cost: t.finalBestBankCost,
  reverse_states_settled: t.reverseStatesSettled,
  reverse_edges_relaxed: t.reverseEdgesRelaxed,
  reverse_pq_pushes: t.reversePqPushes,
  reverse_pq_stale_pops: t.reversePqStalePops,
  reverse_pq_max_size: t.reversePqMaxSize,
  heuristic_seed_count: t.heuristicSeedCount,
  heuristic_component_count: t.heuristicComponentCount,
  heuristic_max_seeds_per_component: t.heuristicMaxSeedsPerComponent,
  heuristic_seeds_per_component_p50: t.heuristicSeedsPerComponentP50,
  heuristic_seeds_per_component_p90: t.heuristicSeedsPerComponentP90,
  heuristic_seeds_per_component_p95: t.heuristicSeedsPerComponentP95,
  heuristic_seeds_per_component_p99: t.heuristicSeedsPerComponentP99,
  heuristic_generator_count: t.heuristicGeneratorCount,
  heuristic_max_generators_per_component: t.heuristicMaxGeneratorsPerComponent,
  heuristic_generators_per_component_p50: t.heuristicGeneratorsPerComponentP50,
  heuristic_generators_per_component_p90: t.heuristicGeneratorsPerComponentP90,
  heuristic_generators_per_component_p95: t.heuristicGeneratorsPerComponentP95,
  heuristic_generators_per_component_p99: t.heuristicGeneratorsPerComponentP99,
  heuristic_generator_seed_ratio_p50: t.heuristicGeneratorSeedRatioP50,
  heuristic_generator_seed_ratio_p90: t.heuristicGeneratorSeedRatioP90,
  heuristic_generator_seed_ratio_p95: t.heuristicGeneratorSeedRatioP95,
  heuristic_generator_seed_ratio_p99: t.heuristicGeneratorSeedRatioP99,
  heuristic_generator_seed_ratio_max: t.heuristicGeneratorSeedRatioMax,
  heuristic_generator_seed_ratio: t.heuristicGeneratorSeedRatio,
  heuristic_calls: t.heuristicCalls,
  heuristic_candidates_scanned: t.heuristicCandidatesScanned,
  heuristic_max_candidates_per_call: t.heuristicMaxCandidatesPerCall
}, null, 2));
JS

echo "Raw result: $output_dir/result.jsonl"
