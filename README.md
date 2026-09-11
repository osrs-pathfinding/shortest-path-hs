# shortest-path-model

The maintained routing implementations are direct Tile A* for current routing
and `ReferenceDijkstra` as a deliberately simple correctness reference. Hierarchical
routing is retained only as archived research and possible future work.

## World facts inspector

Generate the disposable DuckDB inspector database from the authoritative Haskell world model:

```sh
nix-shell --run 'cabal run world-facts'
```

This writes `data/world-facts.duckdb`, containing `metadata`, `components`,
`tiles`, `point_access`, `places`, and the derived `place_facts` view. One point
may have zero, one, or several `point_access` rows/components; these attachments
and structural-reachability flags come directly from `ShortestPath.Topology`.
Query it directly with `duckdb data/world-facts.duckdb`.

## Route benchmarks

### Import benchmark history

Initialize the configured ClickHouse instance with `benchmark-analysis/clickhouse/schema.sql`, then import a JSONL run in one bulk request:

```sh
nix-shell --run 'cabal run bench-import -- --testbed cedric --notes "baseline" out/route-benchmark.jsonl'
```

The importer derives `corpus_id`, `profile_set_id`, and `suite_id`, refuses an existing complete `run_id`, keeps the original JSONL untouched, and stores each original JSON object in `raw_json`. ClickHouse is disposable; archived JSONL files remain the source of truth.

Grafana resources live in `benchmark-analysis/grafana/`. Push them after the
`OSRS Benchmarks` folder exists:

```sh
gcx resources push -p benchmark-analysis/grafana
```

The dashboards require Grafana's image-renderer plugin for `gcx dashboards
snapshot`; the ClickHouse datasource alone is sufficient for normal browsing.

For a weighted-A* sweep, give every imported weight the same sweep ID:

```sh
nix-shell --run 'cabal run route-bench -- --heuristic-weight 1.25 --output out/weight-1.25.jsonl'
nix-shell --run 'cabal run bench-import -- --sweep my-sweep --run-id my-sweep-w1.25 out/weight-1.25.jsonl'
```

`route-bench` is the canonical local runner. It runs in-process (so it does not
measure viewer or HTTP overhead), applies all four account profiles, and writes
one JSON object per route/profile/repetition to a JSONL file.

The checked-in v1 corpus contains 750 fixed routes: 20 smoke routes, 140
standard routes, and 750 full routes. It includes GPS, quest, clue, walking,
transport, Wilderness, geographic, and regression cases. The 40 entries in
`benchmarks/corpus/sentinels-v1.json` are the individually tracked cases.
Ordinary routes contain only structurally reachable endpoints. Intentional
unreachable regression routes must set `expectedReachable: false`.

Regenerate the natural-route selection from the sibling `runelite-gps-plugin`,
`quest-helper`, and `shortest-path` clones, then inspect its endpoint-diversity
report:

```sh
nix-shell --run 'node benchmarks/refine-corpus.js'
nix-shell --run 'node benchmarks/validate-corpus.js'
less benchmarks/corpus/coverage-v1.txt
less benchmarks/corpus/reachability-v1.txt
```

The pinned OSRS Wiki monster-location snapshots used by the generator are stored
in `benchmarks/corpus/wiki-places-v1.json`; generation does not require network
access.

### Validate and smoke-test the v1 corpus

Validate the route schema first:

```sh
nix-shell --run 'node benchmarks/validate-corpus.js'
```

Generate a smoke-tier exact oracle and run the smoke benchmark. Temporary paths
keep generated benchmark data out of the corpus commit:

```sh
nix-shell --run 'cabal run route-bench -- --tier smoke --oracle /tmp/route-bench-smoke-oracle.json --write-oracle --jobs 4'
nix-shell --run 'cabal run route-bench -- --tier smoke --oracle /tmp/route-bench-smoke-oracle.json --output /tmp/route-benchmark-smoke.jsonl --runs 3'
```

### Run standard or full

For a serious comparison, generate the exact oracle after any collision,
transport, requirement, or cost-semantics change. It uses reference Dijkstra and the
full corpus can be slow:

```sh
nix-shell --run 'cabal run route-bench -- --write-oracle --jobs 4'
nix-shell --run 'cabal run route-bench -- --tier standard --runs 5'
nix-shell --run 'cabal run route-bench -- --tier full --runs 3'
```

Normal runs validate every result against the oracle and write
`out/route-benchmark.jsonl`. `--diagnostic` additionally runs reference Dijkstra for
investigation; do not use its timings for performance comparisons.

### Report performance

```sh
node benchmarks/report.js out/route-benchmark.jsonl out/route-benchmark-bencher.json
bencher run --adapter json --file out/route-benchmark-bencher.json
```

The exporter tracks corpus p50/p95/p99, category p50s, and only the pairs in
`benchmarks/corpus/sentinels-v1.json` individually. Bencher owns history and
regression detection.

### Inspect a route in the viewer

Start the viewer, then open <http://127.0.0.1:8000/viewer/>:

```sh
node viewer/server.js
```

The **Test case** menu includes selected corpus routes and latest JSONL results.
Choose a route (a result restores its account profile), then click **Run route**
for its detailed path and counters. The viewer explains individual routes; it
does not provide historical charts.

### Diagnose unreachable routes

The reusable diagnostic executable evaluates the transports on successful paths
against the authoritative account profiles and emits one concise TSV row per
route. Requests must include `id`, `routeId`, `routeName`, and `profile`; extra
fields are ignored:

```sh
nix-shell --run 'cabal run unreachable-diagnostic -- requests.jsonl responses.jsonl unreachable-oracle.jsonl'
```

## Current pathfinder tooling

Build the benchmark executable:

```sh
nix-shell --run 'cabal build exe:pathfinder-tool'
```

Warm or rebuild the tile-A* static cache, including natural walking components and the sparse Manhattan walking network:

```sh
nix-shell --run 'cabal run pathfinder-tool -- tile-static-report'
```

Run correctness checks:

```sh
nix-shell --run 'cabal test tile-astar'
nix-shell --run 'cabal test pathfinder-synthetic'
```

The synthetic suite directly exercises both reverse implementations and checks
the sparse labels against the production clique labels.

Run the Kourend -> Desert benchmark through the direct server protocol:

```sh
printf '%s\n' '{"id":1,"start":{"x":1503,"y":3553,"plane":0},"target":{"x":3359,"y":2912,"plane":0},"allowTransports":true,"includeExpandedTiles":false,"useHeuristic":true,"finder":"tile-full"}' \
  | nix-shell --run 'cabal run pathfinder-tool -- serve'
```

Compare reverse-heuristic implementations:

```sh
# Reference implicit same-component Chebyshev clique.
printf '%s\n' '{"id":1,"start":{"x":1503,"y":3553,"plane":0},"target":{"x":3359,"y":2912,"plane":0},"allowTransports":true,"includeExpandedTiles":false,"useHeuristic":true,"finder":"tile-full"}' \
  | nix-shell --run 'cabal run pathfinder-tool -- serve'

# Sparse Manhattan walking network.
printf '%s\n' '{"id":1,"start":{"x":1503,"y":3553,"plane":0},"target":{"x":3359,"y":2912,"plane":0},"allowTransports":true,"includeExpandedTiles":false,"useHeuristic":true,"finder":"tile-full"}' \
  | nix-shell --run 'SPM_TILE_REVERSE_IMPL=manhattan cabal run pathfinder-tool -- serve'
```

Check sparse Manhattan reverse labels against the clique reference for the query:

```sh
printf '%s\n' '{"id":1,"start":{"x":1503,"y":3553,"plane":0},"target":{"x":3359,"y":2912,"plane":0},"allowTransports":true,"includeExpandedTiles":false,"useHeuristic":true,"finder":"tile-full"}' \
  | nix-shell --run 'SPM_TILE_REVERSE_IMPL=manhattan SPM_TILE_COMPARE_REVERSE=1 cabal run pathfinder-tool -- serve'
```

Optional diagnostic counters:

```sh
SPM_TILE_REVERSE_COUNTERS=1
```

Do not use counter-enabled timings as wall-clock benchmark results; reverse counter threading materially slows the hot loop.

Current useful flags:

```text
SPM_TILE_REVERSE_IMPL=manhattan   use sparse Manhattan reverse Dijkstra
SPM_TILE_COMPARE_REVERSE=1        compare sparse labels against clique labels
SPM_TILE_REVERSE_COUNTERS=1       enable expensive reverse diagnostics
SPM_HEURISTIC_TRANSFORM=c         use C Chebyshev transform for heuristic image rendering
```

These values are parsed once by the executable layer into explicit Tile A*
configuration; routing modules do not read process environment state.

Recent laptop baseline for Kourend -> Desert, 5 warm in-process runs:

```text
Clique median:
  total:   382 ms
  setup:   252 ms
  reverse: 247 ms
  search:  121 ms

Sparse Manhattan CSR median:
  total:   316 ms
  setup:   189 ms
  reverse: 178 ms
  search:  140 ms
```

Static sparse graph size from `tile-static-report`:

```text
static original sites: 7349
static Steiner vertices: 37194
static total vertices: 44543
sparse walking undirected edges: 68667
rough adjacency memory estimate: 2.07 MiB
```
