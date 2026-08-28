# OSRS Pathfinding Prototype — Goal and Scope

## Objective

Build a small, self-contained prototype for experimenting with algorithms for OSRS pathfinding.

The purpose of this project is **not** to reproduce the production Java application. The existing Java code contains substantial application-specific machinery that makes algorithm experimentation awkward.

Instead, this prototype should make it cheap to:

* express the core pathfinding problem accurately;
* implement radically different pathfinding strategies;
* compare them against an exact baseline;
* measure latency, search effort, memory usage, and route quality;
* understand which ideas are worth implementing in the production Java project.

Prefer simple, transparent code over production architecture.

Haskell is a good language for the prototype.

---

## Optimisation goal

The route cost is measured in a single scalar unit: **ticks**.

We want queries to return very quickly. Exact optimality is no longer a hard requirement.

The desired trade-off is:

1. minimise query latency;
2. produce routes that are normally very close to optimal;
3. avoid obviously bad routes that a player would notice;
4. use exact solutions offline as an oracle for evaluating approximations.

Ultimately, we care more about empirical route quality than proving optimality.

For an approximate algorithm, measure at least:

* query latency;
* expanded/searched nodes;
* returned route cost;
* optimal route cost;
* absolute excess ticks;
* stretch:

  `returned_cost / optimal_cost`

Percentiles over a query corpus are more useful than averages alone.

---

# Core problem model

## Walking world

The OSRS world is represented as a sparse graph of walkable tiles.

Walking rules for this prototype:

* start and destination are walkable tiles;
* movement is allowed in any legal one-tile direction, including diagonals;
* every legal walking step has cost `1`;
* diagonal movement also costs `1`;
* running/run energy is not modelled;
* collision determines whether movement between neighbouring tiles is legal.

The coordinate space is roughly 20,000 × 20,000, but most coordinates are not walkable.

The walking graph contains many connected components.

Known approximate component sizes:

* about 1,000 reachable components;
* largest component around 276,000 tiles;
* around the 10th-largest component, sizes are already around 10,000 tiles.

Do not assume we currently know the total number of walkable tiles.

---

## Transports

Non-walking movement is represented explicitly as transport edges.

Examples include:

* ladders;
* boats;
* gates;
* agility shortcuts;
* local transports;
* teleports.

Transport edges may be directed.

Every transport has a fixed base tick cost.

Known rough scale from the real graph:

* about 5,500 simple transport edges;
* about 6,000 naive m-to-n/hub-style transport edges;
* about 400 near-global teleport actions;
* around 13,000 portal/transport edges overall.

These figures are approximate and should not be baked into the algorithm.

Some m-to-n systems may be better represented using explicit hub nodes rather than complete bipartite edge expansion.

---

## Teleport origins

There are two important kinds of teleport:

1. location-specific teleports/transports, usable only from particular locations;
2. teleports which are effectively usable from many or all walking tiles once available.

The latter are an important structural feature of the problem.

Teleport destinations may be random in the real game. For this prototype, assume each teleport lands on one representative destination tile.

---

# Query-time configuration

The underlying map and possible transports are mostly static.

However, each query has a player-specific configuration.

For each teleport/transport, the player may:

* enable it;
* disable it;
* assign an additional non-negative cost penalty.

For an enabled edge:

`queryCost(edge) = baseCost(edge) + queryPenalty(edge)`

where:

`queryPenalty(edge) >= 0`

Disabled edges are unavailable.

The set of enabled transports can vary drastically according to account progression.

This means both usable graph topology and edge weights vary between queries.

An important consequence is that base transport costs are optimistic lower bounds on query-specific transport costs.

---

# Banking state

Some teleports require an item such as an amulet.

Initially, only items/transports currently available to the player can be used.

Once the player reaches a bank, they may collect configured teleport items from the bank. After that point these bank-obtainable teleports can be treated as permanently available for the rest of the route.

There are roughly 100–500 bank locations.

Banking is therefore modelled as a monotonic one-bit state:

`banked = False | True`

A route can transition:

`False -> True`

but never back to `False`.

The search problem can therefore conceptually operate over:

`(location, banked)`

rather than modelling a full inventory.

Do not model consumable-resource subtleties for now. For example, a gate might consume money and technically only be usable once, but this is explicitly out of scope for the prototype.

---

# Query shape

Queries are arbitrary:

`walkable start tile -> walkable destination tile`

They are not normally abstract-node-to-abstract-node queries.

A typical route might be:

`walk -> teleport -> walk -> local transport -> walk`

Therefore the cost of connecting arbitrary concrete source/destination tiles to the transport network is a central part of the problem.

This may be more important for performance than searching the relatively small transport graph itself.

A large walking component can contain hundreds of relevant transport attachment locations. Some components may have around 500 attachment sites.

---

# Prototype architecture

Keep the project deliberately small.

A reasonable rough structure is:

* `World` — map, collision, transports, banks and query-independent data;
* `Query` — enabled edges, penalties and initial availability;
* `Exact` — deliberately correct baseline solver;
* experimental solver modules;
* `Benchmark` — generate/run queries and compare algorithms.

Avoid unnecessary abstraction.

In particular, do not reproduce:

* application configuration infrastructure;
* RuneLite/plugin integration;
* databases unless required to import fixtures;
* dependency injection;
* production logging;
* UI;
* generic graph frameworks unless they materially simplify the experiment.

The entire algorithmic core should remain easy to read.

---

# Exact reference solver

Implement a straightforward exact solver early.

It does not need to be fast enough for production.

Its job is to provide the oracle:

`optimalCost(start, target, config)`

against which experimental algorithms are evaluated.

Correctness and simplicity matter more than performance here.

Use it to validate:

* synthetic test worlds;
* corner cases;
* approximate algorithms;
* preprocessing techniques.

If the exact solver is too slow for a large benchmark corpus, it is acceptable to precompute oracle answers offline or use a smaller representative corpus.

Do not distort experimental algorithms merely to make the exact solver faster.

---

# Test worlds

Support two kinds of datasets.

## 1. Synthetic worlds

Construct tiny worlds designed to expose algorithm behaviour.

Include cases such as:

* multiple disconnected walking components;
* a boat connecting components;
* a global teleport;
* a teleport only available after visiting a bank;
* user penalties changing which teleport is best;
* disabled teleports;
* several candidate attachment points;
* obstacles where geometric distance is a poor guide;
* a nearby-looking portal which is actually worse;
* cases deliberately designed to break pruning heuristics.

These should make algorithm bugs and approximation failures obvious.

## 2. Real OSRS graph fixture

Eventually import a stripped-down export from the production data containing only what is needed for pathfinding:

* walkable tiles / collision;
* transports;
* transport destinations;
* transport base costs;
* banks;
* teleport identities/requirements.

Do not import the Java application architecture into this project.

The prototype should consume a dumb standalone fixture.

JSON is acceptable initially if convenient; a compact binary representation can come later if parsing or size becomes significant.

---

# Experiments we care about

Do not assume the current production architecture is the answer.

The prototype exists specifically so we can experiment with substantially different approaches.

Areas worth investigating include:

* source/destination attachment strategies;
* limiting or ranking candidate transport attachment points;
* A* and weighted A*;
* stronger walking heuristics;
* ALT/landmark techniques;
* component-level abstractions;
* precomputed intra-component information;
* hierarchical routing;
* hub representations for transport systems;
* special handling of global teleports;
* approximate walking distances during high-level search;
* exact refinement only after choosing a high-level route;
* pruning/corridor/beam-style approaches;
* bidirectional methods;
* preprocessing/query-time trade-offs.

Do not implement all of these up front.

The priority is to make each experiment cheap and measurable.

---

# Performance philosophy

Current fresh production performance numbers are unavailable.

Therefore do not optimise against an invented target based on old measurements.

Instead, establish a benchmark harness and measure each algorithm consistently.

A useful eventual goal is interactive latency substantially below 100 ms for normal queries, while accepting somewhat slower difficult cases.

Memory usage matters.

A few GB of preprocessing may be interesting as an experiment to discover the speed/space frontier, but it is probably too large for the final implementation. Prefer approaches that can plausibly fit well below 1 GB unless an experiment demonstrates a compelling reason otherwise.

Expensive static preprocessing is acceptable because the OSRS world graph changes relatively infrequently.

---

# Benchmark reporting

For every serious algorithm experiment, report something like:

* preprocessing time;
* preprocessing memory/disk size;
* median query latency;
* p90/p95/p99 latency;
* worst observed latency;
* number of walking nodes expanded;
* number of abstract nodes expanded;
* median excess ticks over optimal;
* p95/p99 excess ticks;
* median/p95/p99 stretch;
* worst observed stretch;
* examples of the worst-quality routes.

The worst examples matter: an algorithm that is excellent on average but occasionally produces absurd OSRS routes is undesirable.

---

# Guiding principle

Do not prematurely optimise the current Java implementation.

Use this prototype to answer:

**What is the right algorithm for this particular graph?**

The prototype should make it possible to implement an idea, benchmark it against an exact oracle, inspect failures, and replace it with another idea with minimal friction.

Once we find an approach with convincing latency, memory use, and route quality, we can translate that algorithm back into the production Java project.

