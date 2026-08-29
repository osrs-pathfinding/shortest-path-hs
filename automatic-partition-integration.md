# Automatic Partition Integration Specification

## Goal

Compare the current manual walking components with recursive METIS regions and
recursive KaHIP node-separator regions, then integrate either automatic
partition without changing any shortest-route cost.

The partition is metadata over the raw walking graph. It must not make a real
tile unwalkable or invent a cheaper crossing.

## Three comparable views

1. **Manual**: the current reachable-from-Lumbridge census using the three
   configured virtual walls and their cost-1 crossing transports.
2. **METIS**: the raw reachable walking graph, with components over 50,000
   tiles replaced by recursive METIS leaf regions. Every cut walking edge is a
   bidirectional cost-1 interface edge.
3. **KaHIP**: the same raw graph, with oversized components replaced by
   recursive KaHIP leaves. Separator tiles remain explicit interface vertices;
   their original A-S, S-S, and S-B walking adjacencies are preserved.

Components below the partition threshold remain unchanged in the METIS and
KaHIP views so global totals are directly comparable.

## Machine-readable outputs

Keep the existing METIS files and add:

- `out/metis/kahip-partitions.csv`: `component,x,y,plane,region,kind,level`
  where `kind` is `leaf` or `separator`. A tile appears exactly once.
- `out/metis/metis-components.csv` and
  `out/metis/kahip-components.csv`: one row per search region with component,
  region, tile count, bounding box, banks, transport origins, transport
  destinations, global-teleport destinations, distinct interesting tiles, and
  interesting-tile density, plus expanded transport/interface in/out counts
  and distinct incoming/outgoing region counts.
- `out/metis/metis-region-graph.csv` and
  `out/metis/kahip-region-graph.csv`: aggregated directed region connections,
  separated into real `TRANSPORT` and walking `INTERFACE` edges. KaHIP
  separator groups are explicit nodes in this graph.
- `out/metis/partition-census.json`: summaries and region records for manual,
  METIS, and KaHIP, plus KaHIP separator totals.

CSV is the authoritative viewer input; JSON is the compact analysis input.

## Census report

Generate `automatic-partition-census.md` with:

- total represented walkable tiles and search-region count for each method;
- minimum, median, p90, p95, p99, and maximum region sizes;
- singleton and `<=10`, `<=100`, `<=1,000`, `<=20,000`, and `<=50,000`
  region counts;
- the largest 20 regions for each method with the same bank, transport, global
  destination, and interesting-tile columns as the original census;
- per-source-component comparison of original size, resulting region count,
  smallest/largest region, and interface burden;
- METIS cut edges and unique boundary tiles;
- KaHIP unique separator tiles, separator tiles per recursion level, and leaf
  region sizes.

For KaHIP, separator tiles are not search regions. Report them separately and
include them in represented-tile coverage totals.

## Viewer

Replace the METIS-only checkbox with a mode control:

- Manual components
- METIS regions
- KaHIP regions
- Difference

Manual mode keeps the current component overlay. METIS and KaHIP modes colour
leaf regions consistently and show interfaces with high-contrast markers.
Difference mode shows METIS boundaries and KaHIP separators together, with a
small legend and method-specific summary counts. Keep plane and opacity
controls. Selecting a source component must show the same geographic extent in
all modes.

The viewer must not infer region membership from colours or bounding boxes; it
reads the assignment CSVs.

## Integration invariants

Before route integration, validate:

1. every selected raw tile has exactly one METIS leaf assignment;
2. every selected raw tile has exactly one KaHIP leaf or separator assignment;
3. no assignment names a tile outside its source component;
4. METIS boundary rows correspond one-for-one with real raw walking edges;
5. every KaHIP edge is either within a leaf, within the separator interface, or
   connects a leaf to the separator; no raw walking edge directly joins two
   different leaves;
6. represented tile totals equal the raw reachable tile total.

## Route-preservation tests

Use the existing exact Dijkstra implementation as the oracle.

1. A small synthetic graph with four regions, a METIS cut, and a multi-tile
   KaHIP separator must return identical walking costs before and after either
   representation.
2. Sample deterministic real tile pairs within and across each partitioned
   source component. Compare raw walking-only costs with METIS and KaHIP costs.
3. Sample deterministic full-world routes using real transports and compare
   exact costs before and after integration.
4. For every returned partitioned route, expand interface transitions and
   verify that each expanded step is either a legal raw walking edge or the
   original transport with the original cost.

Any cost mismatch is a failed integration; no tolerance is allowed.

## Work split

- **Census/export worker**: extend `app/MetisPartition.hs` only, generate KaHIP
  leaf assignments and all comparison outputs, and add internal coverage checks.
- **Viewer worker**: edit only `viewer/index.html`, `viewer/app.js`, and
  `viewer/styles.css` against the formats above.
- **High-level thread**: review both changes, resolve the shared data contract,
  run Cabal checks and the partition job through `shell.nix`, inspect the
  viewer on port 8080, and then implement route-equivalence tests as a separate
  integration step.

## Acceptance checks for this first fork

- `nix-shell --run 'cabal --config-file=cabal.config.user test'` passes.
- The partition command exports both complete assignment sets without coverage
  or duplicate failures.
- The census report contains all three methods and reconciles represented tile
  totals.
- The viewer switches among all three region views and overlays both interface
  types in Difference mode.
- No production pathfinding algorithm is changed in this fork.

## Commands

`metis-partition integrate` is the normal integration path. It reads the saved
METIS assignments and reconstructs KaHIP leaf membership from the saved
recursive `.separator` files; it must not invoke either partitioner.

Running `metis-partition` with no argument is the explicit, expensive offline
experiment which regenerates all METIS and KaHIP splits. Census, viewer, and
route-integration work should not use it unless repartitioning is intentional.
