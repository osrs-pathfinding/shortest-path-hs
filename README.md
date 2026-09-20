# shortest-path-model

`shortest-path-model` is the Haskell implementation and semantic model for exact,
account-aware Old School RuneScape routing.

The maintained production solver is `TileAStar`; `ReferenceDijkstra` is the
deliberately simple independent correctness oracle. The repository also exposes
a small set of executables for benchmarking, routing-artifact generation,
separator generation, world inspection, and benchmark-account inspection.

For the algorithm and semantic details, see:

* [`ALGORITHM.md`](ALGORITHM.md) — Tile A*, relaxed reverse search, banking,
  Wilderness/global-teleport handling, separators, and correctness invariants.
* [`SEMANTIC_MODEL.md`](SEMANTIC_MODEL.md) — account and transport requirement
  semantics.
* [`docs/routing-static-format-v1.md`](docs/routing-static-format-v1.md) —
  exported static routing-artifact format.

## What this repository exposes

The repository has three distinct surfaces:

| Surface                             | Purpose                                                                                    |
| ----------------------------------- | ------------------------------------------------------------------------------------------ |
| Haskell library under `src/`        | Construct worlds, account-aware routing queries, and exact routes                          |
| Executables under `app/` / `tools/` | Benchmarking and offline model/artifact tooling                                            |
| Tests                               | Cross-check Tile A* against the independent Dijkstra oracle and exercise routing semantics |

The main routing modules are:

```text
ShortestPath.Tile
ShortestPath.Account
ShortestPath.Pathfinder
ShortestPath.World
ShortestPath.Topology
ShortestPath.Exact.TileAStar
ShortestPath.Exact.ReferenceDijkstra
```

`ShortestPath.Exact.TileAStar` is the main routing facade.

## World data

The model does not carry a second copy of the RuneLite pathfinding resource
data. By default it reads the sibling `shortest-path` checkout:

```text
../shortest-path/src/main/resources/
```

In particular, `defaultSourcePaths` refers to:

```text
../shortest-path/src/main/resources/collision-map.zip
../shortest-path/src/main/resources/destinations/game_features/bank.tsv
../shortest-path/src/main/resources/transports/*.tsv
data/routing-separators-v1.json
```

The sibling `shortest-path` checkout contains authoritative world/game data.
`data/routing-separators-v1.json` is the checked-in, Haskell Tile A*-specific
separator artifact derived from that data. It is validated against the current
walking topology before routing starts.

A typical development checkout is therefore:

```text
parent/
  shortest-path/
  shortest-path-model/
  shortest-path-corpus/
```

The benchmark/account tooling also uses the sibling
`../shortest-path-corpus` checkout. Set `SHORTEST_PATH_CORPUS_DIR` to override
that location.

## Routing API: “what is the route between these two points?”

A route query is represented by `ShortestPath.Pathfinder.Query`.

Coordinates are OSRS `(x, y, plane)` coordinates packed with `packTile`:

```haskell
start  = packTile 3221 3218 0
target = packTile 3000 3000 0
```

The simplest exact query uses `defaultQuery`:

```haskell
import ShortestPath.Exact.TileAStar
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  world <- loadWorld defaultSourcePaths
  astar <- buildTileAStar world

  let start = packTile 3221 3218 0
      target = packTile 3000 3000 0
      query = defaultQuery start target

  (route, _timings) <- findRouteProfiledTileAStar astar query

  print (routeCost route)
  mapM_ print (routeSteps route)
```

`Route` contains:

```haskell
data Route = Route
  { routeCost :: Int
  , routeExpandedNodes :: Int
  , routeSteps :: [RouteStep]
  }

data RouteStep
  = Walk Tile
  | UseTransport String Tile
```

Every ordinary walking step costs `1`. Transport edges cost their configured
duration plus any query-specific transport penalty.

An unreachable query is represented by:

```text
routeCost  = maxBound
routeSteps = []
```

### Default query semantics

`defaultQuery start target` means:

* exact search (`heuristicWeight = 1`);
* transports enabled;
* all known transport types enabled except `SEASONAL_TRANSPORTS`;
* no transport penalties;
* bank-path routing enabled;
* transport requirements ignored;
* query time `0`.

Ignoring requirements is useful for topology/algorithm work, but it is not an
account-specific player route.

For an account-aware route, supply an `AccountState`:

```haskell
import ShortestPath.Account

let query =
      (defaultQuery start target)
        { requirementMode = ConfiguredRequirements account
        , queryNowMinutes = nowMinutes
        }
```

The account controls levels, quests, varbits/varplayers, carried items,
equipment, rune pouch, bank, diaries, POH state, planted spirit trees, fairy
rings, spellbook and runtime/cooldown state.

Other query-level controls include:

```haskell
allowTransports       :: Bool
enabledTransportTypes :: Set String
transportPenalties    :: Map String Int
bankPathEnabled       :: Bool
heuristicWeight       :: Double
```

See `ShortestPath.Pathfinder.Query` and `RoutingOptions` for the complete
interface.

### Reusing prepared state

The one-shot call above is the simplest API. Higher-throughput consumers can
reuse progressively more work:

```text
World / WorldTopology / TileAStar
    long-lived static state

RoutingOptions
    -> compiled routing account
    reusable while account/routing configuration is unchanged

target
    -> PreparedTarget
    reusable for multiple starts with the same compiled routing account

start
    -> searchPrepared
    cheap forward search
```

The relevant API is exposed through `ShortestPath.Exact.TileAStar`:

```haskell
compileRoutingAccount
prepareTarget
searchPrepared
```

There are profiled variants for measuring preparation/search phases separately.

## Executables

Build everything with:

```sh
nix-shell --run 'cabal build all'
```

The current executable surface is:

```text
route-bench
route-query
routing-artifact
separator-artifact
world-facts
account-profile
```

### `route-bench`

`route-bench` is the implementation-local benchmark runner. Benchmark campaign
orchestration, result history, ClickHouse/Grafana and comparison tooling live in
the sibling `shortest-path-benchmarks` repository.

Usage:

```text
route-bench
  [--corpus-dir DIR]
  [--corpus PATH]
  [--oracle PATH]
  [--output PATH]
  [--runs N]
  [--tier smoke|standard|full]
  [--limit N]
  [--write-oracle]
  [--jobs N]
  [--diagnostic]
  [--rerun-failures JSONL]
  [--strict-profile-vars]
  [--heuristic-weight N]
```

By default it reads:

```text
<corpus>/corpus/routes-v1.json
<corpus>/oracle/oracle-v1.json
<corpus>/accounts/account-profiles-v1.json
```

and writes:

```text
out/route-benchmark.jsonl
```

Generate an exact reference oracle:

```sh
nix-shell --run 'cabal run route-bench -- --tier smoke --write-oracle --jobs 4'
```

Run the Tile A* benchmark against that oracle:

```sh
nix-shell --run 'cabal run route-bench -- --tier smoke --runs 3'
```

`--diagnostic` additionally executes `ReferenceDijkstra` and is intended for
correctness investigation, not performance measurement.

The executable accepts the following Tile A* diagnostic configuration from the
environment:

```text
SPM_TILE_REVERSE_IMPL=manhattan|clique
SPM_MANHATTAN_GATEWAYS=0|1
SPM_TILE_COMPARE_REVERSE=0|1
SPM_TILE_REVERSE_COUNTERS=0|1
```

The normal/default reverse implementation used by the profiled executable path
is `manhattan` (the sparse walking reverse graph).

### `route-query`

`route-query` runs an account-aware query with one of the canonical corpus
profiles and prints the reconstructed route:

```sh
nix-shell --run 'cabal run route-query -- maxed 3221 3218 0 3000 3000 0'
```

Pass `--counters` to also print compact timing and high-level search metrics.
Use `--corpus-dir DIR` to override the corpus discovery described above.

### `routing-artifact`

`routing-artifact` owns implementation/static-artifact inspection and export.

Usage:

```text
routing-artifact component-transform-report
routing-artifact tile-static-report
routing-artifact export-routing-static [PATH]
```

Examples:

```sh
nix-shell --run 'cabal run routing-artifact -- tile-static-report'
```

```sh
nix-shell --run 'cabal run routing-artifact -- export-routing-static out/routing-static-v1.bin'
```

`component-transform-report` writes CSV/Markdown reports under `out/`.

`tile-static-report` reports the size of the precomputed Tile A* site/sparse
walking structures.

`export-routing-static` writes the portable static artifact documented in
[`docs/routing-static-format-v1.md`](docs/routing-static-format-v1.md).

The executable maintains a generated final-topology cache at:

```text
out/tile-astar-topology-v2.bin
```

It contains the derived routing components, separator crossings, structural
reachability, and compact flat search/site arrays, and rebuilds it when the
source-world fingerprint changes. Set `SPM_CACHE_TIMINGS=1` to print cache
startup phases to stderr.

### `separator-artifact`

`separator-artifact` generates the offline routing-separator artifact consumed
by production topology construction.

Usage:

```text
separator-artifact generate OUTPUT MAXIMUM-SIZE MINIMUM-CHILD MAXIMUM-SEPARATOR IMBALANCE SEED
```

The currently used generation parameters are:

```sh
nix-shell --run \
  'cabal run separator-artifact -- generate data/routing-separators-v1.json 20000 500 32 40 42'
```

Generation invokes KaHIP's `node_separator`. The output stores validated cut
walking edges; runtime routing does not invoke KaHIP.

The generator also writes:

```text
OUTPUT.diagnostics.json
```

Environment overrides for upstream world inputs are:

```text
SPM_RESOURCES_DIR
SPM_COLLISION_ZIP
SPM_BANK_FILE
```

At runtime, `SPM_SEPARATOR_FILE` independently overrides the model-owned
separator artifact.

### `world-facts`

`world-facts` exports the authoritative Haskell topology/model into a disposable
DuckDB database for inspection and corpus-maintenance tooling.

Usage:

```text
world-facts [--output PATH]
```

Default:

```sh
nix-shell --run 'cabal run world-facts'
```

writes:

```text
out/world-facts.duckdb
```

The database includes model-derived tables/views for natural/routing components,
separator crossings, tiles, point attachments, places and transports.

Overrides:

```text
WORLD_FACTS_DB
WORLD_FACTS_GPS_DESTINATIONS
SHORTEST_PATH_CORPUS_DIR
```

This database is generated development data, not canonical corpus data.

### `account-profile`

`account-profile` inspects the current benchmark-account model.

Usage:

```text
account-profile validate early|mid|end|maxed
account-profile compare BEFORE AFTER
account-profile coverage
account-profile vars
```

Examples:

```sh
nix-shell --run 'cabal run account-profile -- validate maxed'
nix-shell --run 'cabal run account-profile -- compare mid end'
nix-shell --run 'cabal run account-profile -- coverage'
```

The canonical exported profile fixture is read from the sibling
`shortest-path-corpus`.

The current Haskell profile-generation/inspection tooling is transitional; the
long-term source of truth is expected to become semantic profile definitions
which generate the corpus fixture.

## Exactness and the reference solver

With `heuristicWeight = 1`, `TileAStar` is the maintained exact solver.

`ReferenceDijkstra` is intentionally much simpler and slower. It uses the same
world/account semantics but an independent search implementation, and is used as
the correctness oracle in tests and benchmark oracle generation.

The main correctness suites include:

```sh
nix-shell --run 'cabal test tile-astar'
nix-shell --run 'cabal test pathfinder-synthetic'
nix-shell --run 'cabal test smoke'
```

Run the full test suite with:

```sh
nix-shell --run 'cabal test all'
```

## Repository boundaries

This repository owns:

* Haskell routing/account/world semantics;
* exact Tile A* and reference Dijkstra;
* topology and algorithm preprocessing;
* the Haskell benchmark executable;
* routing/static artifact generation;
* currently, model-aware world/corpus maintenance tooling.

It does not own:

* canonical route/profile data — `shortest-path-corpus`;
* benchmark orchestration/history/dashboards — `shortest-path-benchmarks`;
* interactive route visualisation — `shortest-path-viewer`;
* Java-specific benchmark/account tooling — `shortest-path-tooling`;
* RuneLite plugin integration — `gps-plugin`.

The current `tools/world-facts` and `tools/corpus-maintenance` code is
model-aware maintenance tooling. It lives here for now because it depends
directly on the authoritative Haskell topology, but it is not part of the core
routing API.

## Development environment

Enter the development shell with:

```sh
nix-shell
```

It supplies the Haskell/Node/DuckDB/KaHIP tooling required by the current
executables and maintenance scripts.

The current `shell.nix` imports the caller's `<nixpkgs>`; it is therefore a
convenient development environment but is **not currently a pinned reproducible
toolchain**.

## Directory guide

```text
src/                         routing/model library
app/                         implementation executables
tools/account-profiles/      benchmark-account inspection/generation
tools/world-facts/           model -> DuckDB inspection tooling
tools/corpus-maintenance/    model-aware corpus maintenance
test/                        correctness and semantic tests
csrc/                        native distance-transform implementation
docs/                        artifact/interface documentation

```
