# Domain-specific A* routing algorithm

This document describes the maintained exact routing model implemented by
`ShortestPath.Exact.TileAStar`, together with the deliberately simple
`ReferenceDijkstra` correctness oracle. It is intended to be sufficiently
precise to serve as a behavioural specification for ports of the algorithm.

The solver is a tile A*, but most of its performance comes from preparing a
small relaxed graph that preserves OSRS transport structure and from keeping
expensive account/target work out of the forward tile-search loop.

## Maintained solvers

There are two maintained exact solvers:

* `TileAStar` is the optimised solver.
* `ReferenceDijkstra` is the independent correctness oracle.

Both use the same authoritative `WorldTopology` and the same prepared transport
availability semantics from `ShortestPath.Pathfinder`, but their queues,
search-state types, predecessor storage, and search loops are independent.
Agreement between them is therefore meaningful correctness evidence.

`ReferenceDijkstra` explicitly models global-teleport capability as a search
state and is particularly useful for validating Wilderness and bank/global
semantics. `TileAStar` implements the same semantics with specialised source-
side/global-activation machinery so the ordinary tile fast path stays small.

## High-level lifecycle

The implementation has four important lifetimes.

```text
world/static lifetime
    World
      -> WorldTopology
      -> TileStatic

account/config lifetime
    RoutingOptions
      -> prepared transport availability
      -> account-specific SiteGraph

 target lifetime
    target tile
      -> TargetOverlay
      -> reverse relaxed search
      -> component heuristic seeds/generators
      -> PreparedTarget

 start/search lifetime
    start tile + SearchOptions + PreparedTarget
      -> forward tile A*
      -> predecessor reconstruction
      -> Route
```

The current Haskell modules corresponding to those stages are:

```text
ShortestPath.Topology
ShortestPath.Exact.TileAStar.Preprocessing
ShortestPath.Exact.TileAStar.RelaxedGraph
ShortestPath.Exact.TileAStar.ReverseSearch
ShortestPath.Exact.TileAStar.Heuristic
ShortestPath.Exact.TileAStar.Search
ShortestPath.Exact.TileAStar.Reconstruct
```

`ShortestPath.Exact.TileAStar` is the orchestration facade.

The start tile is deliberately absent from target preparation. A prepared
target can be reused for multiple starts as long as the effective routing
account/configuration is unchanged.

## Tile and state representation

A tile is packed into an integer as:

```text
bits  0..14: x
bits 15..29: y
bits 30..31: plane
```

Conceptually the concrete tile-search state is:

```text
(tile, banked)
```

and dense state IDs use:

```text
stateId(node, banked) = node * 2 + (banked ? 1 : 0)
```

The `banked` bit is the only persistent account-capability dimension in the
ordinary tile state.

The Wilderness global-teleport implementation can add up to four transient
abstract states to one forward query. These states are not tiles and are
removed during path reconstruction.

## Cost arithmetic

`maxBound :: Int` represents infinity.

Finite edge costs must be non-negative. Cost addition is only valid when both
operands are finite and addition cannot overflow:

```text
addCost(a,b) =
    invalid, if a == INF
          or b == INF
          or b < 0
          or a > INF - b
    a + b, otherwise
```

Failed addition is treated as unreachable/infinite rather than wrapping.

The custom priority queue stores three primitive values per entry and orders
entries lexicographically by:

```text
(priority, cost, state)
```

There is no decrease-key operation. Improved states are pushed again and stale
entries are rejected when popped by comparing the queued `cost` with the
current best cost for that state.

## Authoritative walking topology

`ShortestPath.Topology` owns the walking/topology model.

### Natural components

`topologyNaturalComponents` partitions the authoritative walking graph produced
by `walkingNeighbors`. Natural-component IDs are stable with respect to the
separator optimisation and are used by structural reachability.

### Routing components and separators

`topologyRoutingComponents` starts from the same walking graph but removes
the offline separator-cut adjacencies during flood fill. Each removed walking
adjacency is reintroduced explicitly as a bidirectional `RoutingCrossing` of
cost 1.

Thus separators change the routing-component partition without changing the
represented shortest-path metric.

Separators are generated offline by `separator-artifact` and checked in as the
Haskell implementation's `data/routing-separators-v1.json`. The artifact
records an FNV-based identity of the effective walking graph produced by
`walkingNeighbors`; runtime recomputes that identity before constructing
routing components. A mismatch is fatal: routing topology construction refuses
to continue with stale separators. After the identity check, every cut is also
validated: its endpoints must exist, it must be a real authoritative walking
edge, it must cross natural components, and it must split routing components.

### Point attachments

A routing-relevant point may itself be a walkable tile or may be a blocked
transport endpoint adjacent to one or more walking components.

`pointAttachmentDetails` / `routingPointAttachments` preserve all such
attachments. A blocked transport endpoint is therefore not assigned to an
arbitrary first neighbour.

A point with no walking attachment can still exist as an exact site-graph node
if transports connect to it. This is required for pure transport chains.

### Structural reachability

Structural reachability is distinct from routing-component partitioning.
Production reachability starts from the explicit Lumbridge seed and closes over
allowed transport connectivity and global-teleport destinations while ignoring
configured structural-only exclusions such as seasonal transports.

Search-tile storage is built only for routing tiles whose containing natural
component is structurally reachable. This filtering does not renumber natural
or routing component IDs.

## Static Tile A* preprocessing

`buildTileStatic` converts `WorldTopology` into reusable search/search-heuristic
arrays.

It creates:

* sorted packed search tiles for structurally reachable routing components;
* one routing-component ID per ordinary search tile;
* an 8-bit ordinary-walking mask per search tile;
* pre-resolved north/south dense-node indices used by the forward walk loop;
* a sorted set of static routing sites;
* all routing-component attachments for each site;
* a packed-tile -> static-site index;
* component -> static-site groups;
* structurally reachable banks;
* the static sparse walking network.

Static routing sites are the union of:

* all local/global transport origins and destinations;
* separator-crossing endpoints;
* structurally reachable bank tiles.

The static sparse walking network is built over these sites, grouped by routing
component.

## Account/config preparation

Rich transport requirements are evaluated before the search.

`RoutingOptions` determines:

* whether transports are enabled at all;
* enabled transport types;
* per-transport-type penalties;
* whether bank-path routing is enabled;
* requirement/account mode;
* the current-time value used by requirements.

Preparation produces `QueryTransportAvailability` with six views:

```text
carriedLocalTransports
bankedLocalTransports
carriedGlobalTransports
bankedGlobalTransports
carriedWildernessGlobalTransports
bankedWildernessGlobalTransports
```

The Wilderness-global lists are the already-available global transports whose
`maxWildernessLevel` is at least 30. Normal global lists contain all available
global transports; Wilderness capability determines which list can currently
be activated.

Requirement evaluation, inventory/bank interpretation, quest checks, and
transport-type filtering are therefore absent from the hot forward search.
Only prepared collections and precomputed penalties are consulted there.

An effective routing fingerprint is derived from the routing-affecting options
and prepared transport availability. Search-only options such as heuristic
weight are deliberately outside this fingerprint.

## The account-specific relaxed graph

Target preparation runs over an immutable account-specific `SiteGraph`.

The graph contains:

* every static spatial site;
* zero or more abstract nodes;
* routing-component attachments for spatial sites;
* the static sparse walking network;
* component -> site groups;
* explicit reverse adjacency for non-walking routing edges.

Each graph node has two states, pre-bank and post-bank.

Explicit routing edges are generated for:

1. available local transports in each bank layer;
2. the one-way bank transition;
3. bank-dependent global teleport factoring;
4. separator crossings.

### Local transports

For each available local transport:

```text
(origin, banked) --duration + penalty--> (destination, banked)
```

Requirement filtering has already selected the carried or banked transport set.

### Bank transition

At a structurally reachable bank, when bank routing is enabled:

```text
(bank, False) --0--> (bank, True)
```

There is no reverse capability transition in the real forward problem.

### Bank-dependent global teleports

Banked global teleports would naively create a complete bipartite relation from
all reachable banks to all global destinations. The relaxed graph factors this
through one abstract node:

```text
(bank, False)
    --0-->
(BankedGlobalTeleports, True)
    --teleport cost-->
(destination, True)
```

The abstract hub exists only when:

* transports are enabled;
* bank routing is enabled;
* at least one reachable bank site exists;
* at least one banked global destination exists.

Multiple banked global transports to the same destination are collapsed to the
minimum account-adjusted cost.

The hub represents consumption of the single bank transition/global opportunity
and is never a real route tile.

### Separator crossings

Every offline separator cut is represented in both directions and in both bank
layers with its exact walking cost, currently 1.

## Target overlay

The target is a query-lifetime overlay rather than a mutation of the account
`SiteGraph`.

If the target already exists as a static site, its existing node is used.
Otherwise one synthetic target site is appended logically for the reverse
query.

The overlay records:

```text
target packed tile
target routing-component attachments
target attachment sites with exact Chebyshev costs
target site ID
whether the target is synthetic
```

For a multi-component target, attachment edges are emitted once per shared
site/component relationship rather than losing all but one attachment.

Both bank layers of the target are reverse-search seeds at cost zero.

Target preparation also resolves search-extra tiles/components/sites needed by
the forward search for non-base static sites and the target. The eventual start
may add one more query-local extra tile without rebuilding the target heuristic.

## Relaxed walking metric

Within one routing component the heuristic relaxes collision details and uses
exact same-plane Chebyshev distance:

```text
d((x1,y1),(x2,y2)) = max(abs(x1-x2), abs(y1-y2))
```

Different planes have no direct relaxed walking edge.

Conceptually, if `s` ranges over reachable routing sites in the component, the
heuristic at tile `x` is:

```text
h(x, banked) =
    min_s ( reverseDistance(s, banked)
          + Chebyshev(x, s) )
```

An exact reverse distance for `x` itself is also considered when `x` is a
routing site.

This preserves explicit transport topology while relaxing detailed collision
inside routing components.

## Reverse-search implementations

There are two exact reverse-search implementations.

### Clique/reference reverse search

`reverseDijkstra` / `reverseDijkstraUncounted` treat the same-component relaxed
walking relation conceptually as a clique. When a site state is settled, the
implementation scans all attached sites in the relevant routing component and
relaxes exact Chebyshev edges.

This implementation is simple and is retained as a correctness/reference
backend.

### Sparse walking reverse search

`reverseDijkstraManhattan` represents the same component-wise Chebyshev metric
with a sparse Manhattan network.

Use the coordinate transform:

```text
a = x + y
b = x - y
```

Then:

```text
|a1-a2| + |b1-b2| = 2 * Chebyshev((x1,y1),(x2,y2))
```

The sparse network adds Steiner vertices and projection/chain edges so the
complete same-component walking clique need not be materialised.

Sparse edge weights are therefore in doubled units. Explicit graph-edge and
target-attachment costs are doubled when entering this reverse search. After
Dijkstra, finite labels are divided by two before becoming ordinary routing
costs.

The sparse network is exact for the original routing sites; it is not an
approximation to the heuristic metric.

`TileAStarConfig` defaults to:

```text
SparseWalkingReverse
compareReverseImplementations = False
collectReverseCounters = False
```

and `SPM_TILE_REVERSE_IMPL` defaults to the sparse/manhattan backend.

There is also a comparison mode which computes clique labels and asserts exact
equality for every query routing state.

### Important current entry-point discrepancy

The profiled/configurable preparation path uses `TileAStarConfig` and therefore
uses `SparseWalkingReverse` by default.

The pure `prepareHeuristicFor` path currently calls `reverseDijkstraUncounted`
directly, i.e. the clique/reference backend, and `findRouteTileAStar` reaches
that pure path through `prepareTarget`.

Thus the source currently has two default behaviours depending on entry point:

```text
findRouteTileAStar / prepareTarget       -> clique reverse
profiled/configurable preparation        -> sparse reverse by default
```

Both are intended to produce identical heuristic labels and exact route costs,
but they differ in preparation cost and in whether sparse generator provenance
is available. This distinction should be resolved or deliberately preserved
before treating one backend as the sole Java-port specification.

## Generator provenance and heuristic scan reduction

The sparse reverse search tracks, for every reverse state:

```text
generator origin state
generator weight
```

Walking edges in the sparse/Steiner network and target-attachment edges preserve
the current generator. Explicit routing edges state whether crossing them starts
a new generator.

Current explicit-edge provenance is:

* local transport edges: start a generator;
* bank transition: starts a generator;
* separator crossing: starts a generator;
* bank -> abstract global hub ingress: starts a generator;
* abstract global hub -> destination leg: does **not** start another generator.

The last rule ensures the factored bank-global hub does not invent an
independent geometric cone at the abstract node.

After reverse search, each `(routing component, banked)` bucket receives:

1. the complete raw seed table, containing every finite spatial/site label;
2. when sparse provenance is available, a de-duplicated generator table.

A propagated generator is used only when the sparse/query walking path is also
a direct Chebyshev geodesic in that component and the generator remains in the
same bank layer. Otherwise that site conservatively remains its own generator.
This is important for multi-attachment sites and any non-geodesic sparse path.

Forward heuristic evaluation uses the generator table when it is non-empty and
falls back to the raw seed table otherwise.

The raw seeds remain retained for validation, metrics, and exact lookup tests.

## Heuristic scan kernel

Prepared generator buckets are stored as structure-of-arrays coordinates and
costs. The forward scan computes:

```text
min_i(cost_i + max(abs(x-x_i), abs(y-y_i)))
```

for the bucket corresponding to `(routing component, banked)`.

The Haskell implementation has scalar and SIMD-capable scan paths. SIMD is an
implementation optimisation only; it does not change heuristic semantics and
must not be treated as part of the cross-language specification.

## The pre-bank / post-bank routing worlds

The Boolean state should be understood as selecting between two capability
worlds rather than merely recording historical bank visitation.

### `banked = False`

The player:

* has only carried/equipped item transports;
* may still reach a bank;
* may gain banked-item access once;
* may still exploit the bank-dependent global opportunity.

### `banked = True`

The player:

* has bank-accessible transports;
* has already crossed the banking transition;
* cannot cross back to the pre-bank world;
* cannot use the one-shot bank/global transition again.

The reverse relaxed graph computes labels for both layers.

## Dynamic bank-global dominance in forward A*

The forward search tracks:

```text
bestBankCost = minimum discovered g-cost at an unbanked reachable bank
```

For an unbanked state with:

```text
cost > bestBankCost
```

that branch's future use of the shared bank-global opportunity cannot improve
on taking the opportunity from the already discovered cheaper bank.

The physical branch is not discarded because its current tile may still be
useful. Instead its future bank-global opportunity is considered dominated.

For such a state the search evaluates both heuristic layers:

```text
hPre  = h(tile, False)
hPost = h(tile, True)
```

and uses:

```text
max(hPre, hPost)
```

with `hPost` ignored only if it is unreachable.

`hPost` is safe here as an optimistic relaxation of the continuation after the
dominated global opportunity is removed: it generously grants banked-item
access immediately. Both terms are lower bounds, so their maximum is also a
lower bound.

This gives the search three effective phases without adding a third concrete
state:

```text
pre-bank, bank-global opportunity live
pre-bank, bank-global opportunity dominated
post-bank
```

The same dominance fact suppresses banked-global relaxations from a bank when
the current bank is strictly more expensive than `bestBankCost`.

Because `bestBankCost` can improve after a state has already entered the
priority queue, a popped state recomputes the effective heuristic. If its new
`g + w*h` is greater than the queued priority, the state is reinserted with the
stronger key before it is expanded.

## Global teleports and Wilderness capability

Global teleports are source/capability opportunities rather than reusable local
edges from every tile.

The current capability function has three values:

```text
NoGlobals
WildernessGlobals
AllGlobals
```

`globalCapabilityAt` implements the OSRS Wilderness regions used by the model:

* deep Wilderness: `NoGlobals`;
* level-21-to-30 region: `WildernessGlobals`;
* outside that restriction: `AllGlobals`.

The prepared Wilderness-global list contains globals usable through level 30;
the ordinary global list contains all account-available globals.

### Starting outside restrictions

If the start has `AllGlobals`, the forward search is seeded with:

```text
start at cost 0
+
each available carried-global destination at teleport cost
```

No extra Wilderness capability states are allocated and the search runs the
unrestricted specialised loop.

### Starting with `WildernessGlobals`

The search seeds the carried level-30-capable global destinations immediately
and allocates transient capability hubs for later activation of the full global
set.

### Starting with `NoGlobals`

No global destination is seeded. The ordinary tile search proceeds until a
settled/relaxed path reaches a tile where `globalCapabilityAt` is stronger.

The search then adds a zero-cost transition into an abstract capability hub:

```text
(tile, banked)
    --0-->
WildernessHub(banked)
```

or:

```text
(tile, banked)
    --0-->
AllGlobalsHub(banked)
```

The hub expands the corresponding prepared global-transport list and returns to
ordinary tile states at the teleport destinations.

These hubs are reached through normal relaxation and therefore preserve the
cheapest path to the first *useful* capability point. The algorithm does not
commit to the first capability boundary discovered in search order.

Local transports can also move directly from a restricted tile to an
unrestricted tile; global activation then occurs at that destination exactly as
it would after walking there.

Capability is tracked separately for pre-bank and post-bank states, so the same
mechanism applies to carried and banked global lists.

### Keeping Wilderness checks off the normal fast path

The forward loop has two specialisations:

```text
goAll        / unrestricted
goRestricted / Wilderness-restricted
```

They share an inlinable `goWith restricted` body, but `restricted` is fixed for
each recursive loop. The unrestricted path therefore does not need to perform
per-state `globalCapabilityAt` activation work.

The restricted search calls `relaxGlobalActivation` after ordinary neighbour
relaxation.

Once a global hub teleports to an unrestricted destination, the resulting tile
state remains in the restricted queue implementation for that query, but it can
activate/use the required global capability through the hub machinery. The
semantic result matches explicit capability-state Dijkstra.

### Restricted-query heuristic cap

The account/target relaxed graph intentionally does not include the start's
Wilderness restriction as another persistent graph dimension. Consequently the
normal relaxed heuristic can assume access to globals earlier than the concrete
restricted state permits.

For restricted forward queries the search additionally computes, for each bank
layer:

```text
globalBound =
    min_global( teleportCost(global)
              + heuristic(destination(global), banked) )
```

and uses:

```text
min(effectiveHeuristic, globalBound)
```

while in the restricted loop. This is an optimistic lower bound and preserves
admissibility while capability activation is modelled explicitly by the
forward search.

## Forward search space

The main search space consists of the static reachable tile arrays plus a small
sorted set of query extras.

A prepared target contributes target/static-site extras. If the start is not
already present in either set, it is inserted for this search together with its
routing-component attachments.

For ordinary base tiles, walking neighbours are expanded using the precomputed
walking mask and dense north/south node indices. This avoids repeated packed
coordinate -> node map lookups for normal movement.

Blocked/non-base query extras use authoritative `walkingNeighbors` at runtime. A
blocked tile can be entered when it is a usable local-transport origin,
preserving transport endpoint semantics.

Every normal walk has cost 1.

## Forward relaxations

For a tile state the search may relax:

1. ordinary walking edges;
2. the zero-cost bank transition;
3. available local transports;
4. unrestricted bank-global transports when legal and not dominated;
5. in a restricted query, zero-cost activation into Wilderness/global hubs.

Transport cost is:

```text
duration + account/configured transport-type penalty
```

A neighbour is only committed when the new `g` is strictly lower than the
stored best cost and its heuristic is finite.

## Sound unreachable pruning

The relaxed graph is constructed as an optimistic superset of real
continuations. Therefore:

```text
no relaxed path to target
```

implies:

```text
no represented real path to target
```

A missing heuristic is not converted to zero. Such a state is pruned.

The implementation distinguishes instrumentation for:

* a tile with no resolved routing-component attachment;
* a tile/component with no finite reverse seed.

This pruning is one of the reasons the solver can avoid flooding structurally
unreachable regions.

## Weighted A*

`SearchOptions` currently contains only `heuristicWeight`.

Queue priority is:

```text
g + round(weight * h)
```

saturated to infinity through the normal safe-cost arithmetic.

`weight = 1` is ordinary exact A*. Weights greater than 1 are an explicitly
non-exact performance mode and are not part of the exact Java-port parity
contract unless selected by the caller.

## Path reconstruction

For every improved ordinary state the forward search records:

```text
previous state
previous transition kind
transport label, when applicable
```

Transition kinds are interpreted as:

```text
0 -> Walk currentTile
1 -> UseTransport label currentTile
other -> emit no RouteStep
```

The no-output case is used for abstract capability transitions such as
Wilderness/global hubs. Thus abstract implementation nodes never appear in the
returned route.

The bank transition is currently recorded through the same zero-cost edge
machinery with an empty label/transport kind in Tile A*, while the public route
semantics are ultimately compared by cost and visible movement/transport
steps. A port should preserve the intended user-visible route semantics rather
than expose abstract capability nodes.

## Correctness checks and tests

The most important maintained cross-checks are:

* Tile A* route cost against `ReferenceDijkstra` on synthetic and corpus cases;
* sparse reverse labels against clique reverse labels;
* raw seed heuristic against generator-reduced heuristic;
* multi-component blocked-endpoint/target attachment tests;
* pure transport-chain tests;
* bank-global abstract-hub tests;
* Wilderness first-available-global tests, including competing activation
  paths and banked globals;
* prepared-target reuse from multiple starts;
* separator-topology equivalence.

The benchmark corpus additionally records reachability, route cost, timings,
queue/search counts, heuristic scan counts, and reverse-search metrics.

## What is semantic and what is only an optimisation

The following are semantic/correctness requirements:

* authoritative walking connectivity;
* separator crossings preserving removed walking edges exactly;
* all valid endpoint attachments;
* account-filtered transport availability and penalties;
* the pre-bank/post-bank capability transition;
* one-way banking;
* bank/global opportunity semantics;
* Wilderness capability restrictions and first-available activation;
* exact target attachment semantics;
* admissible heuristic values for exact A*;
* overflow-safe cost arithmetic;
* unreachable pruning only when the relaxed graph proves no continuation;
* reconstruction that hides abstract nodes.

The following are performance representations and may change in a port:

* Haskell vectors/ST;
* component-site clique versus sparse Manhattan reverse implementation, provided
  labels remain equal;
* Steiner-vertex layout;
* generator scan SIMD;
* structure-of-arrays versus another primitive-array layout;
* exact heap storage implementation, provided ordering/stale-entry semantics
  needed for deterministic parity are retained;
* separator-generation algorithm, once the validated separator artifact is
  fixed.

## Porting invariants

A Java implementation should preserve the following until differential parity
with Haskell is established:

1. **Tile packing.** Use the same 15/15/2 coordinate bit layout. In Java the
   packed value is naturally a signed 32-bit `int`; plane-2/3 values can be
   negative. Any operation that depends on the Haskell packed-`Int` numeric
   ordering must therefore be reviewed explicitly rather than accidentally
   using signed Java ordering.

2. **State layering.** Every spatial routing/search node has distinct pre-bank
   and post-bank states with the same one-way capability semantics.

3. **Safe infinity arithmetic.** Never allow integer wraparound to create a
   finite route from an infinite/overflowing cost.

4. **Priority queue behaviour.** Use duplicate pushes plus stale-cost rejection.
   For deterministic comparison, order equal entries by `(priority, cost,
   state)` as the Haskell heap does.

5. **Exact transport costs.** Apply duration plus the already-prepared
   transport-type penalty exactly once.

6. **Blocked endpoint attachments.** Preserve every authoritative adjacent
   routing component. Never select an arbitrary first neighbour.

7. **Target overlay.** A non-static target is a query-local synthetic site with
   all valid component attachments; target preparation must not depend on the
   start tile.

8. **Reverse labels.** The sparse walking network is an exact representation of
   the clique Chebyshev relaxation. Sparse and clique reverse labels for routing
   states must agree.

9. **Generator reduction.** Generator provenance is an optimisation of
   heuristic evaluation, not a change to heuristic values. The generator scan
   must equal the complete raw-seed lower envelope.

10. **Bank-global hub.** Factoring banked globals through an abstract node must
    preserve the complete-bank-to-destination shortest costs and must not create
    a second independent generator at the hub's destination leg.

11. **Dynamic bank dominance.** `bestBankCost` strengthens the heuristic and
    suppresses only the dominated future bank-global opportunity; it does not
    discard the physical unbanked branch.

12. **Wilderness semantics.** Global teleports become available at the cheapest
    reachable point where the relevant capability is legal, not merely at the
    initial tile. Local transports can cross capability boundaries. Preserve
    separate carried/banked and level-30/all-global capability behaviour.

13. **Restricted heuristic admissibility.** Preserve the restricted-query
    `min(effective heuristic, global bound)` logic or prove an equivalent lower
    bound if the Java model changes how global capability is represented.

14. **Unreachable pruning.** `INF` heuristic means prune; do not silently turn
    it into zero.

15. **Abstract-state reconstruction.** Wilderness/global capability hubs and
    other non-user-visible abstract states must not appear as route steps.

16. **Prepared lifetimes.** Keep world/static, account/config, target, and
    start/search data separable so an unchanged account can reuse its graph and
    an unchanged target can be searched from multiple starts.

17. **Reference parity first.** Before Java-specific optimisation, compare
    synthetic cases and corpus route costs against the Haskell implementation
    and/or `ReferenceDijkstra`. Optimise representations only after semantic
    parity is established.
