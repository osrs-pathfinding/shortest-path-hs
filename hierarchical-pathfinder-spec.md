# Exact KaHIP Hierarchical Pathfinder Specification

## Objective

Build an exact hierarchical solver over the existing KaHIP decomposition and
prove, by differential testing, that its route cost equals a flat exact search
over the same raw walking graph and transport semantics.

The first milestone targets the semantics implemented by the current Haskell
prototype. It does not silently add Java-production semantics that the flat
oracle cannot yet express.

## Correctness baseline

Both solvers must use:

- `walkingNeighborsRaw`, not the manual virtual-wall topology;
- the same reachable-from-Lumbridge tile scope used by the partition census;
- the same enabled transport types and query penalties;
- all current local transport edges except `VIRTUAL_WALL`;
- the current global-teleport semantics;
- identical banking behavior.

Create a raw flat oracle rather than changing the existing manual-wall
`Dijkstra` behavior in place. Every hierarchical test compares against this
raw oracle.

The current Haskell model does not yet enforce item/quest/var requirements,
bank-obtainable teleport changes, wilderness teleport eligibility, or one-use
hub semantics. Those features require matching changes to the flat oracle and
query model before the hierarchy may implement them.

For milestone 1:

- keep hub/network transports as their existing expanded edges;
- keep `banked` in the search state for structural parity, but do not invent
  capabilities it does not currently unlock;
- model global teleports at the query start and every explicit arrival/state
  transition node. This is exact under the current universal-availability
  model because walking first cannot improve use of the same teleport;
- do not implement `usedHubMask` until the oracle has the same rule.

## Core data contract

### Partition index

Add `ShortestPath.Hierarchy.Partition` with small concrete types:

```haskell
data LeafId = LeafId Int String
  deriving stock (Eq, Ord, Show)

data TileClass
  = LeafTile LeafId
  | SeparatorTile String Int
  deriving stock (Eq, Ord, Show)

data Partition = Partition
  { tileClasses :: IntMap TileClass
  , leafTileSets :: Map LeafId IntSet
  , separatorTileSet :: IntSet
  }
```

`loadPartition` reads `out/metis/kahip-partitions.csv`, rebuilds raw walking
components, keeps only components reachable from Lumbridge through current
real transports/global destinations, and assigns every unpartitioned reachable
component to one `LeafId component ("raw-" <> show component)`.

Required invariants:

- every reachable raw walkable tile has exactly one class;
- every CSV tile belongs to its declared raw component;
- every leaf tile appears in exactly one `leafTileSets` entry;
- `separatorTileSet` is exactly the set of `SeparatorTile` entries;
- no `VIRTUAL_WALL` topology or transport participates.

The loader must expose a constructor from in-memory assignments so synthetic
tests do not depend on OSRS files.

### Hierarchical nodes

Add `ShortestPath.Hierarchy.Types` only when the partition contract has landed:

```haskell
data Node
  = Terminal Tile
  | Separator Tile
  | QuerySource
  | QueryTarget
  deriving stock (Eq, Ord, Show)

data TerminalKind
  = BankTerminal
  | LocalTransportOrigin
  | LocalTransportDestination
  | GlobalTeleportDestination
  | RegionGateway
  deriving stock (Eq, Ord, Show)
```

A terminal is keyed by tile and stores a set of kinds. There is never more
than one terminal node for the same tile.

Separator tiles remain individual `Separator Tile` nodes; separator labels are
metadata, not compressed pathfinding nodes.

### Preprocessed overlay

```haskell
data LeafOverlay = LeafOverlay
  { leafTerminals :: Map Tile (Set TerminalKind)
  , leafDistances :: Map (Tile, Tile) Int
  }

data Hierarchy = Hierarchy
  { hierarchyPartition :: Partition
  , leafOverlays :: Map LeafId LeafOverlay
  , terminalLeaf :: Map Tile LeafId
  }
```

Only distances are stored. Concrete path witnesses are reconstructed later by
leaf-local BFS.

## Terminal discovery

For every leaf, deduplicate these tiles:

- bank tiles;
- local transport origins and destinations, excluding `VIRTUAL_WALL`;
- global teleport destinations;
- leaf tiles adjacent through a legal raw walking edge to a separator tile.

Do not add every walkable tile as a terminal. Do not add eligibility-boundary
terminals until the query model has a real eligibility predicate.

Validate that every local transport endpoint inside the indexed world is
either a leaf terminal or an explicit separator tile.

## Leaf metric closure

For each terminal, run a mutable-queue BFS restricted to its leaf's `IntSet`.
Record exact distances to other terminals in that leaf. Store one canonical
pair for symmetric walking distances.

The BFS must not visit separator tiles or another leaf. Every stored distance
must correspond to a legal raw walking path of the same length.

Preprocessing reports progress per leaf and records:

- leaf tiles;
- terminal count and gateway count;
- BFS count and expanded tiles;
- distance-entry count;
- elapsed time.

Serialization is not required for the first synthetic milestone. Add it only
when real-world preprocessing time makes repeated runs inconvenient; use the
already installed `binary` package rather than a custom format.

## Exact hierarchical query

Add `ShortestPath.Exact.Hierarchical` implementing `RouteFinder`.

For each query:

1. Classify source and target.
2. If an endpoint is a leaf tile, run one leaf-local BFS to its terminals.
3. Include a direct source-to-target leaf-local candidate when both endpoints
   share a leaf.
4. Run Dijkstra over terminal nodes, explicit separator tiles, and temporary
   source/target nodes.
5. Expand leaf metric edges, legal separator walking edges, local transports,
   and current global teleports with the same filtering/cost as the raw oracle.
6. Preserve `banked :: Bool`; apply dominance only for the same spatial node
   and otherwise identical state.
7. Reconstruct selected leaf segments with leaf-local BFS and assert that the
   concrete route cost equals the abstract result.

No A*, pruning, hub compression, separator compression, or approximate metric
closure belongs in this milestone.

## Instrumentation

Extend `Route` minimally only if necessary. Prefer a solver-specific stats
record returned by an additional function over adding hierarchy fields to all
pathfinders.

Capture:

- source/target attachment expansions and time;
- abstract states popped and edges relaxed;
- metric, interface, separator, local-transport, and global edges considered;
- reconstruction BFS count/expansions/time;
- total time.

Instrumentation must not affect route cost or queue ordering.

## Validation sequence

### Synthetic gate

Build one in-memory world containing:

- four leaf regions;
- a multi-tile separator with separator-to-separator walking;
- duplicate terminal roles on one tile;
- a same-leaf source/target path that should not touch a terminal;
- a directed local transport;
- one bank;
- one global teleport destination;
- a route that leaves and re-enters a leaf.

Assertions:

- partition coverage and terminal deduplication;
- exact leaf metric distances;
- legal gateway/separator adjacencies;
- equal flat/hierarchical costs for each explicit case;
- reconstructed route cost equals abstract cost.

### Real differential gate

Use a deterministic seed and preserve failing cases as text/JSON. Start with:

- 10 walking-only pairs in each partitioned source component;
- 10 cross-region walking pairs where a route exists;
- 50 full-transport queries across the reachable world;
- enabled-all, disabled-subset, and large-penalty configurations.

Stop on the first mismatch and report source, target, query, costs, flat route,
abstract route, and region sequence. Increase the corpus only after this gate
is clean.

## Luna work packages

Work packages are sequential unless explicitly marked parallel. Agents edit
only their assigned files and report commands, files, assumptions, and risks.

### H0: Semantics audit (parallel with H1)

Write scope: `hierarchy-model-audit.md` only.

Inspect the current Haskell query/oracle and the Java source for:

- global teleport eligibility/wilderness state;
- bank-obtainable item transitions;
- requirement evaluation;
- logical hub families, cost decomposition, and reuse semantics;
- differences caused by manual virtual walls.

Deliver a table of `main-algorithm-plan.md` requirements with status:
`implemented`, `missing`, or `requires oracle change`, plus exact source paths
and a recommended milestone order. Do not edit code.

### H1: Partition index foundation (parallel with H0)

Write scope: `src/ShortestPath/Hierarchy/Partition.hs` only.

Implement the partition types, CSV parser using existing text/TSV/container
libraries, raw reachable component indexing with mutable BFS queues, complete
coverage validation, and an in-memory synthetic constructor. Reuse
`walkingNeighborsRaw`; exclude virtual walls. Do not implement terminals,
metrics, or search.

Checks: a module-local `validatePartition` plus a tiny synthetic four-region
check callable from tests. Do not rerun KaHIP.

### H2: Terminal discovery and local BFS

Dependency: H1 API reviewed and integrated.

Write scope:

- `src/ShortestPath/Hierarchy/Types.hs`
- `src/ShortestPath/Hierarchy/Preprocess.hs`

Implement terminal discovery, gateway detection, leaf-restricted mutable BFS,
metric closure, local path reconstruction, and preprocessing statistics. Do
not implement Dijkstra or transports. Supply pure/in-memory constructors so
tests need no OSRS files.

### H3: Raw flat oracle

Dependency: H0 decisions reviewed. May run parallel with H2.

Write scope: `src/ShortestPath/Exact/RawDijkstra.hs` only.

Extract the current Dijkstra behavior into a raw-walking oracle using
`walkingNeighborsRaw` and excluding `VIRTUAL_WALL`. Do not change existing
`Dijkstra`. Keep query filtering, penalties, globals, and bank state identical.

### H4: Hierarchical exact search

Dependencies: H2 and H3 integrated.

Write scope: `src/ShortestPath/Exact/Hierarchical.hs` only.

Implement source/target attachment, same-leaf direct candidate, abstract
Dijkstra, explicit separator traversal, metric edges, expanded local
transports, current global teleports, banking-state dominance, predecessor
tracking, and concrete reconstruction. No hubs or new query semantics.

### H5: Synthetic correctness suite

Dependency: H4 integrated.

Write scope: `test/HierarchySynthetic.hs` only.

Implement the synthetic gate cases above. Assertions compare route costs and
validate reconstructed route steps. Do not read OSRS resource files.

### H6: Real differential runner

Dependency: synthetic gate passes.

Write scope: `app/HierarchyDifferential.hs` only.

Load current OSRS data and saved KaHIP assignments, build the hierarchy once,
run deterministic query corpora, stop/dump on mismatch, and print preprocessing
and query instrumentation summaries. Never invoke METIS or KaHIP.

Include a named smoke corpus using coordinates from
`../shortest-path-tooling/src/test/resources/dashboard/unit-tests.csv`. These
are concrete OSRS regression routes, but milestone 1 compares their
hierarchical costs with `RawDijkstra`; it must not assert the Java
`expected_length` values until inventory, requirements, wilderness eligibility,
and Java cost semantics exist in both Haskell solvers. The runner must require
an explicit option for the larger corpus and print progress per route.

## High-level thread responsibilities

The high-level thread owns:

- this specification and API decisions;
- Cabal module/executable/test registration;
- integrating each sequential fork and resolving type contracts;
- deciding H0 semantic gaps before H3/H4;
- running `shell.nix` builds/tests;
- executing the real preprocessing/differential runner with progress updates;
- reviewing every mismatch rather than weakening assertions;
- final documentation and measured results.

## Milestone acceptance

Milestone 1 is complete only when:

- partition coverage equals the current 1,346,259 reachable raw tiles;
- no partitioner was invoked;
- all synthetic cases pass;
- the initial real differential corpus has zero cost mismatches;
- every hierarchical route reconstructs to legal raw walking/transport steps;
- preprocessing and query statistics are reported;
- existing manual-wall `Dijkstra` and production algorithm behavior remain
  unchanged.
