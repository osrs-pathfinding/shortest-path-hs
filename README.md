# shortest-path-model

An exact, account-aware Old School RuneScape pathfinder. The maintained solver
is direct Tile A*; `ReferenceDijkstra` is the deliberately simple correctness
oracle.

> Partition artifact configuration: maximum component size `20000`, minimum
> child size `500`, maximum separator size `32`, imbalance `40`, random seed
> `42`.

## Requirements

Enter `nix-shell` for the pinned development tools. Runtime world data is read
from the sibling `../shortest-path` checkout, and benchmarks use the sibling
`../shortest-path-corpus` checkout. Override the corpus location with
`SHORTEST_PATH_CORPUS_DIR`.

## Repository boundaries

`src/` is the reusable routing/model library and `app/` contains supported
implementation executables such as `route-bench` and routing-artifact
preprocessing. `tools/` contains offline model-aware maintenance tooling:
world facts, corpus endpoint maintenance, and account-profile generation.

Canonical routes and exported profiles belong to the sibling
[`shortest-path-corpus`](../shortest-path-corpus) data repository. Benchmark
campaigns/analysis and the interactive viewer belong to
[`shortest-path-benchmarks`](../shortest-path-benchmarks) and
[`shortest-path-viewer`](../shortest-path-viewer).

Corpus maintenance defaults to the sibling corpus. `refine-corpus.js` also
accepts `GPS_PLUGIN_DIR`, `QUEST_HELPER_DIR`, and `SHORTEST_PATH_DIR` when those
source checkouts are not at their default sibling locations.

## World facts inspector

Generate the disposable DuckDB inspector database from the authoritative Haskell world model:

```sh
nix-shell --run 'cabal run world-facts'
```

This writes `out/world-facts.duckdb`, containing `metadata`, natural `components`,
`routing_components`, `separator_crossings`, `tiles`, `point_access`, `places`,
and the derived `place_facts` view. One point
may have zero, one, or several `point_access` rows/components; these attachments
and structural-reachability flags come directly from `ShortestPath.Topology`.
Query it directly with `duckdb out/world-facts.duckdb`. Set `WORLD_FACTS_DB` to
write somewhere else.

## Offline routing separators

The distributed `routing-separators-v1.json` is generated offline with KaHIP:

```sh
nix-shell --run 'cabal run separator-artifact -- generate ../shortest-path/src/main/resources/routing-separators-v1.json 20000 500 32 40 42'
```

Each KaHIP vertex is one collision-walkable tile. Each undirected graph edge is
one legal adjacency returned by `walkingNeighborsRaw` whose other endpoint is
in the same natural component. Oversized natural components are partitioned
independently with `node_separator --preconfiguration=strong`; the portable
JSON stores only canonical cut tile pairs, the walking-topology identity, the
settings, and the format version. Only structurally reachable natural
components above the size threshold are candidates. The threshold triggers an
attempt rather than requiring a split: branches remain intact when either
child is below 500 tiles or the separator exceeds 32 tiles. Generation metrics
and the attempted component IDs/sizes are written beside the artifact as
`.diagnostics.json`. Runtime never invokes KaHIP and rejects a missing,
mismatched, or invalid artifact.

## Route benchmarks

Campaign orchestration, result importing, ClickHouse analysis, Grafana
resources, and profiling scripts live in the sibling
[`shortest-path-benchmarks`](../shortest-path-benchmarks) repository.
`route-bench` remains here with the implementation it measures.

`route-bench` is the canonical local runner. It runs in-process (so it does not
measure viewer or HTTP overhead), applies all four account profiles, and writes
one JSON object per route/profile/repetition to a JSONL file.

The sibling v1 corpus contains 726 fixed routes and 2,904 route/profile
cases: 2,811 positive and 87 expected-unreachable negatives. It includes GPS, quest, clue, walking,
transport, Wilderness, geographic, and regression cases.
The 26 unresolved all-profile failures live in
`../shortest-path-corpus/corpus/excluded-routes-v1.json` and are not executed. Every result
records its corpus `expectation` separately from observed and oracle reachability.

Regenerate the natural-route selection from the sibling `runelite-gps-plugin`,
`quest-helper`, and `shortest-path` clones, then inspect its endpoint-diversity
report:

```sh
nix-shell --run 'node tools/corpus-maintenance/refine-corpus.js'
node ../shortest-path-corpus/tools/validate.js
```

### Validate and smoke-test the v1 corpus

Validate the route schema first:

```sh
node ../shortest-path-corpus/tools/validate.js
```

Generate a smoke-tier exact oracle and run the smoke benchmark. Temporary paths
keep generated benchmark data out of the corpus commit:

```sh
nix-shell --run 'cabal run route-bench -- --corpus-dir ../shortest-path-corpus --tier smoke --oracle /tmp/route-bench-smoke-oracle.json --write-oracle --jobs 4'
nix-shell --run 'cabal run route-bench -- --corpus-dir ../shortest-path-corpus --tier smoke --oracle /tmp/route-bench-smoke-oracle.json --output /tmp/route-benchmark-smoke.jsonl --runs 3'
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
node ../shortest-path-benchmarks/scripts/report.js out/route-benchmark.jsonl out/route-benchmark-bencher.json
bencher run --adapter json --file out/route-benchmark-bencher.json
```

The exporter tracks positive-corpus p50/p95/p99 and category p50s, with negative
search aggregates separately. Bencher owns history and regression detection.

### Inspect a route in the viewer

The interactive viewer now lives in the sibling
[`shortest-path-viewer`](../shortest-path-viewer) repository.

The **Test case** menu includes selected corpus routes and latest JSONL results.
Choose a route (a result restores its account profile), then click **Run route**
for its detailed path and counters. The viewer explains individual routes; it
does not provide historical charts.

### Diagnose unreachable routes

Benchmark mismatch and unreachable-route diagnostics live in the sibling
[`shortest-path-benchmarks`](../shortest-path-benchmarks) repository.

## Current routing-artifact tooling

Build the benchmark executable:

```sh
nix-shell --run 'cabal build exe:routing-artifact'
```

Warm or rebuild the tile-A* static cache, including natural walking components and the sparse Manhattan walking network:

```sh
nix-shell --run 'cabal run routing-artifact -- tile-static-report'
```

Run correctness checks:

```sh
nix-shell --run 'cabal test tile-astar'
nix-shell --run 'cabal test pathfinder-synthetic'
```

The synthetic suite directly exercises both reverse implementations and checks
the sparse labels against the production clique labels.

Do not use counter-enabled timings as wall-clock benchmark results; reverse counter threading materially slows the hot loop.

Current useful flags:

```text
SPM_TILE_REVERSE_IMPL=manhattan   use sparse Manhattan reverse Dijkstra
SPM_TILE_COMPARE_REVERSE=1        compare sparse labels against clique labels
SPM_TILE_REVERSE_COUNTERS=1       enable expensive reverse diagnostics
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
