# Domain-specific A* optimisations

The pathfinder is a direct tile A*, but much of its performance comes from modelling OSRS-specific transport structure in the heuristic and search state.

## Maintained solvers

The runtime has exactly two maintained routing implementations:

* `TileAStar` is the current optimised solver.
* `ReferenceDijkstra` is the deliberately simple correctness oracle.

Both consume account-filtered transports from `prepareQueryTransports` and the
authoritative `WorldTopology`, plus shared transition helpers in
`ShortestPath.Pathfinder`. Their queues, distance
maps, predecessor storage, state representation, and search loops remain
independent so agreement is meaningful correctness evidence.

## Tile A* implementation pipeline

The implementation is split by its existing algorithmic stages:

```text
Prepared query + WorldTopology
    -> TileAStar.RelaxedGraph
    -> TileAStar.ReverseSearch
    -> TileAStar.Heuristic
    -> TileAStar.Search
    -> TileAStar.Reconstruct
```

`ShortestPath.Exact.TileAStar` is the public facade and owns orchestration.
`TileAStar.Preprocessing` builds reusable static site data, while
`TileAStar.Debug` owns reverse-path inspection and viewer rendering. Process
environment settings are parsed by `TileAStar.Configuration` and passed into
the algorithm explicitly.

The hot paths intentionally retain packed `Int` states, dense IDs, unboxed
vectors, mutable ST loops, and a specialised growable heap. These structures
avoid per-transition allocation; the module boundaries do not generalise the
solver into a generic graph framework.

## Authoritative world topology

`ShortestPath.Topology` owns the maintained topology model:

* `NaturalComponents` assigns every ordinary walkable tile a stable component
  ID derived only from walking connectivity.
* `topologyRoutingComponents` applies the distributed offline separator cuts
  while flood-filling the same walking graph. With no cuts it is identical to
  the natural decomposition; a cut adjacency becomes a bidirectional cost-1
  `RoutingCrossing`, so no walking edge is removed.
* `pointAttachments` maps any routing-relevant point to zero, one, or multiple
  natural components using the real endpoint-access semantics.
* `StructuralReachability` is a separate set over those stable IDs. Production
  reachability uses the explicit Lumbridge seed and rejects a missing seed.

Sparse Manhattan networks, reverse candidate grouping, and forward heuristic
lookup use routing-component IDs. Structural reachability and endpoint access
remain defined in terms of natural components.

Offline KaHIP generation considers only structurally reachable natural
components above its configured size threshold. That threshold only triggers
a separator attempt. A proposed split is discarded, leaving the branch whole,
when a child is too small or the separator exceeds the configured interface
limit.

Account-specific transport availability and benchmark eligibility are higher
layers; neither changes natural-component identity. `world-facts` reads this
same model rather than reconstructing components or point adjacency.

## Virtual walls

`VIRTUAL_WALL` records support the older wall-aware topology exposed by
`walkingNeighbors`. Current routing instead uses `walkingNeighborsRaw`, whose
natural walking graph does not apply those manual barriers. Consequently
`VIRTUAL_WALL` records are not legal transport relaxations in either maintained
solver. The structural-reachability policy excludes them explicitly.

## Transport-aware component heuristic

Ordinary geometric distance is too weak for OSRS: the best route may initially walk away from the target to reach a teleport, bank, fairy ring, etc.

The heuristic therefore solves a relaxed transport problem backwards from the target.

Within a natural walking component, walking is relaxed to exact Chebyshev distance:

```text
d(p,q) = max(|dx|, |dy|)
```

Cross-component movement still requires an explicit transport.

For a tile `x`, the heuristic is conceptually:

```text
h(x) =
    min_s (
        Chebyshev(x, s)
        + relaxedDistanceToTarget(s)
    )
```

over transport-relevant sites `s` in the same natural component.

This retains important transport topology while ignoring detailed collision inside each component.

## The Boolean: pre-bank vs post-bank worlds

The most important domain-specific state extension is:

```haskell
State Tile Bool
```

The Boolean is called `banked`, but its importance is clearer if viewed as selecting one of **two routing worlds**.

### `False`: pre-bank

The player:

* has only carried/equipped item transports;
* may still reach a bank;
* may then gain banked-item access;
* may still use the one-shot bank-global opportunity.

### `True`: post-bank

The player:

* has bank-accessible item transports;
* has already crossed the banking transition;
* cannot use the bank-global opportunity again.

The relaxed heuristic graph has the same two layers.

Ordinary transport edges stay within a layer:

```text
False --carried transport--> False

True  --banked transport-->  True
```

Banking is a one-way transition:

```text
False --bank--> True
```

and a bank-global teleport is also:

```text
False @ bank --global teleport--> True @ destination
```

There is deliberately no `True -> False` transition.

This tiny Boolean therefore captures a large amount of OSRS routing structure without making inventory or individual teleport items part of the A* state.

The reverse search computes distances for **both layers**, and every component therefore has both pre-bank and post-bank heuristic seeds.

## Dynamic meaning of the post-bank heuristic

The particularly useful optimisation is that the two heuristic layers do not have to be queried only according to the concrete search state's Boolean.

While searching, the algorithm tracks:

```text
bestBankCost =
    cheapest g-cost at which any bank has been reached
```

Suppose an unbanked state has:

```text
g >= bestBankCost
```

Its future use of the shared bank-global opportunity cannot beat using that same opportunity from the already-discovered cheaper bank.

The branch itself is not dominated: its physical location may still matter.

What is dominated is specifically:

```text
this branch's future bank-global opportunity
```

At this point the algorithm evaluates both heuristic layers:

```text
h_pre  = heuristic(tile, False)
h_post = heuristic(tile, True)
```

and uses:

```text
h = max(h_pre, h_post)
```

Why is `h_post` valid even though the real state has not banked yet?

Because once bank-global use has been proven irrelevant, the `True` heuristic is an **optimistic relaxation** of the remaining branch: it removes the dominated bank-global opportunity but generously pretends that banked-item transports are already available at the current tile.

It can therefore underestimate the real restricted continuation, but cannot overestimate it.

Both `h_pre` and `h_post` are lower bounds, so their maximum is also a lower bound.

This is effectively a third semantic situation:

```text
pre-bank, global opportunity available
pre-bank, global opportunity dominated
post-bank
```

without introducing a third concrete A* state.

That is one of the main domain-specific optimisations in the current search.

The same dominance information is also used to suppress strictly more expensive bank-global transitions at banks.

Because `bestBankCost` can improve after a state has entered the priority queue, popped states are re-evaluated and re-keyed when the stronger heuristic now gives them a larger priority.

## Initial global teleports are source-side choices

Carried global teleports are not treated as reusable edges everywhere in the heuristic graph.

Instead the search is initially seeded with:

```text
start normally
```

and:

```text
each available carried global teleport destination
```

This models them as source-side opportunities and avoids carrying another lifecycle dimension through the entire search.

Bank-dependent globals are handled separately by the Boolean transition described above.

## Sound unreachable pruning

The relaxed graph is intended to contain every real continuation.

Therefore:

```text
no relaxed path to target
```

implies:

```text
no real path to target
```

A missing heuristic value is consequently not converted to `h = 0`; the state is discarded.

Tile search storage is derived from the components marked structurally
reachable; this filtering does not create or renumber component IDs.

Together these rules prevent A* from flooding disconnected or unsupported regions.

## Component-indexed heuristic work

Transport sites are grouped under every natural component they attach to.

Reverse relaxed walking therefore considers only sites in the current component rather than scanning every transport site in the world.

Likewise, the final heuristic seed table is indexed by:

```text
(component, banked)
```

so tile evaluation only scans relevant seeds.

This was an important practical reduction in heuristic setup/evaluation work.

## Transport availability is prepared once

Rich OSRS requirements are evaluated before the search through `prepareQueryTransports`.

The hot search consumes prepared sets for:

```text
carried local transports
banked local transports
carried globals
banked globals
```

rather than repeatedly interpreting skills/items/quests and other requirement expressions while expanding tiles.

## Blocked transport origins

Some routing sites are blocked tiles. Such a site retains every adjacent
natural-component attachment; no first-neighbour choice is made. A site with no
walking attachment remains an exact site-graph node, allowing pure transport
chains while genuinely unreachable sites still receive no reverse distance.

## Sparse walking network

The code also contains an exact sparse Manhattan-network representation of same-component Chebyshev connectivity.

It transforms Chebyshev geometry into Manhattan geometry and introduces Steiner vertices to avoid the conceptual complete walking clique.

This remains an optional reverse-search implementation (`SPM_TILE_REVERSE_IMPL=manhattan`) isolated in `TileAStar.ReverseSearch`, with an assertion mode for checking that its labels match the production clique implementation. The separate counted clique loop is retained only for diagnostics because threading counters changes measured performance.

It should therefore be considered an implemented alternative backend, not yet the default core algorithm.

## Guiding idea

The major gains are not exotic changes to A* itself.

They come from representing a small amount of OSRS-specific future state:

```text
natural component
+
transport topology
+
one banked Boolean
+
global dominance information
```

strongly enough that A* can avoid exploring large parts of the tile graph.

The bank Boolean is especially important: rather than merely recording whether the player has visited a bank, its two heuristic layers provide reusable lower bounds corresponding to different capability relaxations. Dynamic dominance can then combine those bounds to represent a third effective phase without paying for another search-state dimension.
