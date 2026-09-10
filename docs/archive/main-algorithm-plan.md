# Build an Exact Hierarchical OSRS Pathfinder (Historical)

This plan describes a removed prototype and is retained only as research history.

## Goal

Implement an **exact hierarchical pathfinder** on top of the KaHIP leaf-region decomposition.

This is the next algorithm prototype. Do not approximate or prune yet.

The purpose is to prove that we can replace large parts of the concrete walking graph with a much smaller hierarchical representation while returning **exactly the same route cost** as a flat exact search.

The flat exact pathfinder remains the correctness oracle.

The key principle is:

> Compress static walking inside a region, but keep every location where a route can enter, leave, change movement mode, change capability, or change search state explicit.

If the hierarchical solver disagrees with the flat exact solver, treat that as a bug in the hierarchy.

---

# 1. Spatial decomposition

Use the existing KaHIP decomposition.

Every walkable tile should belong to exactly one of:

1. a KaHIP leaf region; or
2. a KaHIP separator/interface set.

Do not turn KaHIP separator tiles into blocked terrain.

They remain ordinary walkable OSRS tiles.

Conceptually:

```text
leaf region A
      |
separator/interface tiles
      |
leaf region B
```

The hierarchy must preserve exactly the original walking cost through this structure.

---

# 2. Leaf-region terminal nodes

Inside each leaf region, identify every tile at which something relevant can happen.

Call these **terminals**.

A terminal includes at least:

* a tile containing a local transport origin;
* a local transport destination;
* a bank;
* a global/broad teleport destination;
* a leaf tile adjacent to a KaHIP separator tile;
* any tile needed to represent transitions in global-teleport eligibility;
* any other tile where query/search capability can change.

Deduplicate terminals spatially.

A tile may simultaneously be:

* a bank;
* a transport endpoint;
* a global destination;
* a region gateway.

It should still be represented as one spatial terminal with multiple properties.

---

# 3. Region gateways

For every ordinary walking edge:

```text
u -> s
```

where:

* `u` is a leaf-region tile;
* `s` is a KaHIP separator tile;

make `u` a region gateway/terminal.

Preserve the original walking adjacency explicitly:

```text
terminal(u) --cost 1--> separator(s)
```

and the reverse direction if the original walking graph permits it.

Do not approximate boundary cost.

Do not replace an arbitrary number of crossing edges with a single synthetic crossing unless it is provably equivalent.

---

# 4. Preserve the separator graph explicitly

Separator tiles should initially remain explicit graph nodes.

Preserve:

* separator -> separator walking edges;
* separator -> leaf gateway edges;
* leaf gateway -> separator edges.

This is deliberately conservative.

There are few enough separator tiles that there is no need to compress them immediately.

Later we can investigate compressing separator groups as another optimisation.

For the exact baseline, keeping them concrete reduces the chance of introducing subtle distance errors.

---

# 5. Precompute exact walking distances inside each leaf

For every leaf region `R`, let:

```text
T(R)
```

be its set of terminals.

Compute the exact walking distance between every pair of terminals in the same region, with search restricted to the ordinary walking tiles belonging to that leaf.

Conceptually build the metric closure:

```text
t1 ------- exact walking distance ------- t2
```

for every reachable pair:

```text
t1, t2 ∈ T(R)
```

Walking remains:

* 8-directional;
* legal collision only;
* cost 1 per step including diagonal movement.

A simple BFS from each terminal is acceptable for preprocessing.

This is offline preprocessing. Optimising preprocessing time is not currently important.

Do not run through separator tiles when computing a leaf's internal metric closure.

Separator traversal remains explicit in the hierarchical graph.

---

# 6. Do not store complete concrete paths initially

For each precomputed intra-region connection, storing:

```text
(fromTerminal, toTerminal, distance)
```

is sufficient.

Do not initially store the full tile-by-tile path for every terminal pair.

That may consume unnecessary memory.

For final route reconstruction, after the high-level route has been selected, rerun a local BFS inside the relevant leaf to reconstruct each chosen walking segment.

Thus:

```text
preprocessing:
    store distances

query:
    select abstract route

reconstruction:
    rerun local BFS for only the chosen segments
```

This keeps the correctness model simple while avoiding large path-witness storage.

---

# 7. Resulting static walking overlay

After preprocessing, the walking part of the high-level graph consists of:

```text
region terminal
    |
    | precomputed exact distance
    v
region terminal
```

plus:

```text
region gateway
    |
    | original cost-1 walking edge
    v
separator tile
```

plus the concrete walking graph between separator tiles.

This transformation must be lossless.

It must remain possible for a route to:

* leave a region;
* later re-enter the same region;
* cross several separator groups;
* walk between any relevant transport endpoints;

without changing its optimal cost.

---

# 8. Local transports

Represent ordinary local transports explicitly.

Examples include:

* ladders;
* boats;
* gates;
* shortcuts;
* fixed-origin teleports.

Their origin and destination tiles are already terminals.

Add directed abstract edges corresponding exactly to the original transport.

At query time:

* remove disabled transports;
* use their configured query cost;
* add query-specific penalties.

The query cost remains:

```text
base cost + non-negative configured penalty
```

Do not precompute query-specific transport weights.

Only walking distances are static.

---

# 9. Hub transports

Represent each logical m-to-n transport family with abstract hub nodes rather than complete bipartite expansion where doing so preserves the transport's exact cost semantics.

Current examples include systems such as:

* fairy rings;
* spirit trees;
* minecarts;
* gnome gliders;
* etc.

Conceptually:

```text
origin A ----\
origin B ----- HUB ---- destination X
origin C ----/    \---- destination Y
                   \--- destination Z
```

Before replacing an expanded transport family by one hub node, verify that its cost matrix can actually be represented this way.

For example, a simple hub representation is exact if pairwise costs decompose as something like:

```text
entryCost(origin) + exitCost(destination)
```

or if the game's semantics give every choice the same hub traversal cost.

If an existing transport family has genuinely origin/destination-specific costs that cannot be represented by one hub node, do not force the abstraction.

Use:

* several abstract nodes; or
* keep that family expanded.

Correctness matters more than reducing edge count at this stage.

---

# 10. Enforce one-use hub semantics explicitly for now

The intended route semantics include the property that a logical hub transport should not be used repeatedly.

There are currently very few logical hub families, so for the exact prototype it is acceptable to encode this explicitly.

For example:

```text
usedHubMask :: Word8
```

for the current eight logical hub systems.

On entering/using hub `H`:

```text
require H not already used
set bit H
```

This potentially multiplies the theoretical state space by 256, but this is acceptable for an exact reference implementation and greatly simplifies reasoning about correctness.

Do not yet spend time proving this state can be eliminated.

That is a later optimisation experiment.

---

# 11. Banking state

Search state must include:

```text
banked :: Bool
```

Banking is monotonic:

```text
False -> True
```

and never:

```text
True -> False
```

At a bank terminal, permit the banking transition with whatever tick cost the current model assigns to banking.

After banking, configured bank-obtainable teleports become available permanently.

Walking and ordinary transports preserve `banked`.

Thus a high-level search state is initially something like:

```text
State
    { location    :: AbstractNode
    , banked      :: Bool
    , usedHubMask :: HubMask
    }
```

Keep this simple.

---

# 12. Banking-state dominance

Implement the already identified dominance rule.

At the same abstract spatial/search node and with otherwise equivalent state:

```text
(node, True)
```

has a superset of the capabilities of:

```text
(node, False)
```

Therefore if:

```text
costTrue <= costFalse
```

the `False` label is dominated and can be discarded.

In particular, once:

```text
(node, True)
```

has been settled by Dijkstra at cost `g`,

any later:

```text
(node, False)
```

label with cost >= `g` is unnecessary.

Apply this only where the rest of the state, such as `usedHubMask`, is compatible.

Do not use an unsafe dominance rule across different hub-use masks.

---

# 13. Global/broad teleports

Do not expand broad teleports from every walkable tile.

Model them with an abstract mechanism similar to hub transports.

Conceptually:

```text
current eligible location
          |
          v
   GLOBAL TELEPORT HUB
       /    |     \
      /     |      \
    T1      T2      T3
     |       |       |
    dest1   dest2   dest3
```

The outgoing teleport actions are filtered and weighted according to the query configuration and banking state.

Important: broad/global teleports are **not actually usable from every OSRS tile**.

The abstract representation must preserve the predicate:

```text
canUseGlobalTeleport(tile)
```

or its actual equivalent in the existing data model.

---

# 14. Global-first dominance

Use the previously established property:

> If a global teleport is available and usable at the current tile/state, and walking does not change either condition, there is no reason to walk before using that global teleport.

Therefore do not represent a global-teleport edge from every ordinary tile.

Global teleport use only needs to become possible at places where the search can newly acquire a relevant opportunity, including:

* the query start, if global teleports are usable there;
* arrival from another transport;
* arrival at a bank / change to `banked=True`;
* crossing from a global-disabled area into a global-enabled area;
* other explicit capability-changing terminals.

Every transport destination is already a terminal, so arriving by transport gives us an explicit location at which global teleport availability can be checked.

---

# 15. Global-teleport eligibility boundaries

Be careful if a leaf region contains both:

```text
global teleports allowed
```

and:

```text
global teleports forbidden
```

walking tiles.

For exactness, walking from a forbidden area into an allowed area can create a new teleport opportunity.

Therefore identify boundaries where the global-teleport eligibility predicate changes.

The allowed-side tile of such a crossing must become a terminal.

Then a route such as:

```text
start in forbidden area
    -> walk
    -> enter allowed area
    -> global teleport
```

remains representable.

If convenient, it may instead be cleaner to refine/split regions so that global-teleport eligibility is homogeneous within each region.

Either approach is acceptable.

Do not silently assume every KaHIP leaf has uniform global-teleport eligibility.

---

# 16. Query source attachment

The source is an arbitrary walkable tile.

If the source lies inside an ordinary leaf:

1. run one local BFS from the source restricted to that leaf;
2. compute exact distance from the source to every terminal in that leaf;
3. use those distances as temporary edges/seeds into the hierarchical search.

Conceptually:

```text
source
  | \
  |  \
 d1  d2 ...
  |    \
 t1    t2
```

Also check whether global teleport use is immediately available at the source.

If the source itself is:

* a terminal;
* a separator tile;

attach it directly.

Do not permanently insert arbitrary query sources into preprocessing.

---

# 17. Query target attachment

Do the symmetric operation for the arbitrary destination.

If the target lies inside a leaf, obtain exact local distances between that target and the leaf's terminals.

These become temporary sink connections from the hierarchy to the target.

If walking adjacency is represented as directed for any reason, compute these using the correct reverse graph rather than assuming symmetry.

If the target is a separator/interface tile, attach it directly.

---

# 18. Same-region direct route

It is essential to preserve the possibility that source and target are in the same leaf and the optimal route never touches a precomputed terminal.

Therefore either:

* make source and target temporary terminals and compute their direct local distance; or
* explicitly compute the direct source -> target leaf-local walking route as a candidate.

Example:

```text
source ------------> target
```

must not be forced to detour through a bank, transport endpoint or boundary terminal.

This candidate competes normally with routes that leave the region and come back.

---

# 19. High-level exact search

Run Dijkstra over the resulting query-specific hierarchical state graph.

Do not use A*, weighted A*, beam search or approximate pruning yet.

The graph contains:

* precomputed exact intra-leaf walking edges;
* concrete separator walking edges;
* explicit region-gateway walking edges;
* local transport edges;
* abstract hub nodes;
* abstract global teleport mechanism;
* query source/target attachment edges;
* banking state;
* explicit one-use hub state.

All edge weights must be non-negative.

The result should therefore be an exact shortest path in the transformed graph.

If the transformation is lossless, this must equal the flat exact solver.

---

# 20. Route reconstruction

The high-level search returns an abstract route such as:

```text
source
-> regional terminal
-> separator
-> regional terminal
-> transport
-> regional terminal
-> target
```

For every abstract intra-region walking edge selected:

```text
terminal A -> terminal B
```

rerun a local BFS inside that leaf to obtain the actual tile sequence.

For source and target attachment segments, do the same.

Explicit transports and separator walking edges already have concrete representations.

The reconstructed route must have exactly the high-level cost.

Add an assertion checking that.

---

# 21. Correctness invariant

The hierarchy should satisfy:

> For every concrete route in the original graph, there exists a route through the hierarchical representation with the same cost, and vice versa.

The important reason this should hold is that every point where a concrete route can leave a leaf or take a non-walking action is represented as a terminal.

Inside a leaf, any concrete walking subpath between two such terminals can be replaced by the precomputed shortest walking distance between those terminals.

Conversely, every precomputed metric edge corresponds to a real walking path.

Do not introduce an abstraction that violates this invariant.

---

# 22. Validation against flat exact search

This is essential.

For every test query compute:

```text
flatCost
hierarchicalCost
```

and require:

```text
flatCost == hierarchicalCost
```

Do this before benchmarking speed.

Test multiple query configurations, including:

* most transports enabled;
* many transports disabled;
* large transport penalties;
* minimal account/unlocks;
* mature account/unlocks;
* teleports initially available;
* teleports only available after banking.

---

# 23. Important correctness cases

Construct explicit tests for:

### Pure walking

```text
source -> target
```

inside one leaf.

### Crossing a separator

```text
leaf A -> separator -> leaf B
```

### Leaving and re-entering a leaf

The hierarchy must permit this when optimal.

### Local transport

```text
walk -> transport -> walk
```

### Hub transport

Use one logical hub once.

### Banking

```text
walk -> bank -> newly available teleport
```

### Banking dominance

Confirm dominated `banked=False` states are discarded without changing the result.

### Global teleport from start

```text
start -> global teleport -> ...
```

### Global teleport after banking

```text
start -> bank -> global teleport -> ...
```

### Global teleport after entering an allowed area

Start somewhere that forbids broad teleports:

```text
forbidden
-> walk across eligibility boundary
-> global teleport
```

### Global teleport forbidden area

Verify the hierarchy cannot teleport directly from the forbidden source.

### Query endpoint on separator tile

Test source and destination independently.

### Query endpoint on a transport terminal

Test source and destination independently.

### Path uses several leaf regions

Exercise multiple separator levels.

---

# 24. Random differential testing

Once hand-written cases pass, perform differential testing.

Generate random pairs of reachable walkable tiles.

For each query:

1. choose/generate a query transport configuration;
2. solve with flat exact pathfinder;
3. solve with hierarchical exact pathfinder;
4. compare costs.

Run enough tests to cover all large partitioned components and many small components.

When a mismatch occurs, dump:

* source;
* target;
* query configuration;
* flat cost;
* hierarchical cost;
* flat route;
* abstract route;
* region IDs traversed;
* banking transitions;
* hub uses;
* global teleport uses.

Stop and investigate mismatches rather than treating them as noise.

---

# 25. Instrumentation

Although performance is not the first goal, collect useful measurements from the start.

For each query record:

### Source attachment

* leaf size;
* terminals in leaf;
* BFS tiles expanded;
* time.

### Target attachment

Same metrics.

### Abstract search

* abstract states popped;
* abstract states relaxed;
* transport edges considered;
* region metric edges considered;
* separator nodes visited;
* time.

### Reconstruction

* number of local BFS runs;
* tiles expanded;
* time.

### Total

* complete query latency.

This will tell us where the next optimisation should focus.

---

# 26. Preprocessing statistics

Report:

* total leaf regions;
* terminals per leaf;
* median/p95/max terminals per leaf;
* region gateways per leaf;
* global-eligibility terminals per leaf;
* number of pairwise intra-region distance entries;
* memory used by those entries;
* preprocessing BFS count;
* preprocessing time.

Also identify regions that dominate preprocessing/storage.

Do not assume the ~40k-tile regions are necessarily the expensive ones: terminal count may matter more than tile count.

---

# 27. First implementation can favour simplicity

It is acceptable for the first exact hierarchy to use:

* pairwise terminal metric closures;
* ordinary priority queues;
* explicit separator nodes;
* an 8-bit used-hub mask;
* repeated BFS during route reconstruction;
* relatively expensive offline preprocessing.

Do not introduce sophisticated compression until correctness is established.

The first milestone is:

```text
flat exact cost == hierarchical exact cost
```

for a substantial test corpus.

---

# 28. What not to optimise yet

Do not yet implement:

* weighted A*;
* ALT;
* heuristic pruning;
* approximate terminal selection;
* beam search;
* lossy region compression;
* approximate walking distances;
* partial metric closures;
* separator pruning;
* aggressive state-dominance beyond clearly proven rules.

Those are follow-up experiments.

The exact hierarchy is the baseline against which those ideas will be judged.

---

# 29. Main experiment questions

Once this works, report:

1. Does the hierarchical representation reproduce flat shortest-path costs exactly?
2. How many terminals exist per leaf?
3. How large is the complete intra-leaf metric closure?
4. How expensive is source attachment?
5. How expensive is target attachment?
6. How large is the high-level search?
7. How often does the search touch more than a few leaf regions?
8. Which regions dominate query time?
9. Does the open component 3607 behave noticeably worse than the low-separator regions?
10. Is the exact hierarchy already fast enough to be interesting before approximations?

The immediate goal is not production performance.

The immediate goal is to establish a **correct, measurable hierarchical model of the OSRS graph** on which we can safely experiment.
