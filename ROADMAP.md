# Roadmap

This project aims to become a production-quality OSRS pathfinder with a reproducible benchmark suite, accurate account/transport modelling, and eventually a public web interface for exploring routes under realistic player state.

## Current state

* Direct tile A* is the primary routing implementation.
* The heuristic is transport-aware and based on a relaxed component/transport graph.
* Natural walking components, transport topology, bank/global lifecycle, and unreachable-state pruning are implemented.
* Canonical Early/Mid/End/Maxed account profiles exist for testing.
* The benchmark corpus contains roughly:

  * 750 routes
  * 4 account profiles
  * ~3,000 route/profile cases
* The corpus has been cleaned so ordinary benchmarks use currently supported/reachable endpoints.
* `world-facts.duckdb` provides queryable derived facts about world topology, components, places, and structural reachability.
* Benchmark results are emitted as canonical JSONL.
* Results can be imported into ClickHouse and explored/comparison-tested in Grafana.
* The existing viewer supports spatial inspection of individual routes/search behaviour.

## Implementation strategy

The current implementation is in Haskell, but Haskell is not necessarily the final production language.

Treat the Haskell implementation primarily as:

* the place to develop and validate the algorithm;
* the semantic/reference implementation;
* the environment for experimenting with heuristics, lifecycle modelling and transport semantics;
* the source of correctness oracles and benchmark data.

Do not assume that maximising Haskell-specific performance is a long-term project goal.

### Java / RuneLite

A Java implementation will eventually be required for a RuneLite plugin.

The algorithm should therefore remain portable enough that its important ideas can be reproduced in Java without depending on Haskell-specific abstractions or runtime behaviour.

Before a Java port:

* stabilise the routing semantics;
* make the benchmark corpus green for correctness;
* identify the algorithmic structures that matter for performance;
* document the cost model and heuristic precisely;
* retain cross-language benchmark/reference fixtures.

The Java implementation should be checked against the same corpus and expected route costs as the Haskell implementation.

### Rust / production backend

Rust is a likely candidate for a future standalone/production implementation.

Reasons include:

* simpler deployment and operational model than a Haskell runtime;
* predictable memory representation;
* strong support for mutable arrays, heaps, hash tables and other structures used heavily by graph search;
* easier control over allocations and data layout;
* broad systems/performance library ecosystem.

A Rust implementation may ultimately be a better target for serious low-level optimisation than Haskell.

### Implication for optimisation work

Distinguish:

```text
algorithmic optimisation
    useful in every implementation

representation/runtime optimisation
    potentially language-specific
```

Prioritise work such as:

* better heuristics;
* fewer expanded states;
* improved dominance rules;
* better transport modelling;
* reducing asymptotic setup work;
* avoiding unnecessary graph/state dimensions;
* improved preprocessing.

Be more cautious about spending large amounts of time on:

* Haskell-specific allocation tricks;
* specialised mutable-container tuning;
* GHC-specific representation hacks;
* RTS tuning;
* low-level changes whose benefit would disappear in Java/Rust.

Haskell performance still matters enough to keep development iteration practical and to expose genuine algorithmic bottlenecks, but it does not need to become the ultimate performance ceiling.

### Cross-language validation

Longer term, the benchmark corpus should make it possible to compare:

```text
Haskell reference/prototype
Java RuneLite implementation
Rust production implementation
other external implementations
```

on the same route/profile definitions.

Correctness should be comparable across languages using:

* reachability;
* route cost;
* expected/reference cost.

Performance comparisons should distinguish machine/testbed effects and implementation-language differences.

The benchmark corpus is therefore also the migration path away from Haskell: it allows future implementations to be validated against the semantics developed here without relying on the Haskell code itself.


## Near term

### Correctness

Make the full benchmark corpus correct before further serious optimisation.

* Run all ~3,000 cases against the trusted/reference solver.
* Classify failures:

  * wrong path cost
  * false unreachable
  * false reachable
  * transport/requirement mismatch
  * lifecycle/state modelling bug
  * endpoint/component bug
  * world-data/model bug
* Fix semantic classes of failures rather than individual benchmark cases.
* Treat correctness as a gate for performance comparison.

### Requirement and account fidelity

Finish making transport availability match realistic OSRS account state.

Important areas:

* skills
* quests
* diaries
* inventory
* equipment
* bank contents
* spellbook/runtime state
* POH facilities
* permanent transport unlocks
* relevant varbits/varplayers

Maintain one authoritative requirement evaluator shared by all routing implementations.

Keep account-static requirement handling out of the hot search loop where possible.

### World-model completeness

Use `world-facts.duckdb` as an inspection tool to find places which should be reachable but are not.

Keep this separate from benchmark eligibility.

The broad place catalogue should continue to contain currently unsupported locations, including future Sailing destinations.

## Medium term

### Evidence-driven optimisation

Once correctness is stable:

* establish stable benchmark baselines;
* compare changes across the full corpus;
* use Grafana to inspect:

  * candidate vs baseline scatter
  * ranked regressions
  * distributions
  * profile/category breakdowns
  * individual benchmark history;
* use states expanded and related counters alongside wall time;
* optimise based on broad evidence rather than individual example routes.

Likely areas include:

* heuristic setup cost
* reverse relaxed search
* tile-search hot-loop representation
* transport preparation
* PQ behaviour
* account-specific transport filtering

### KaHIP component partitioning

Future experiment: explore using KaHIP to partition the natural-component/transport graph into
decent-sized regions separated by small cuts. The purpose is to find useful
boundaries for hierarchical search and preprocessing, not to change routing
semantics.

Prototype this offline from `world-facts.duckdb` and measure:

* region balance and cut size;
* the number and cost of transport edges crossing each cut;
* whether common benchmark endpoints are distributed usefully;
* setup cost, expanded states, and route cost with the resulting hierarchy.

Try node and edge weights which reflect component size, transport frequency,
and transport cost. Reject partitions which create tiny or transport-heavy
regions, and retain reference Dijkstra as the correctness oracle
until a decomposition proves useful across the corpus.

### Wilderness semantics

Model Wilderness teleport restrictions accurately.

Prefer treating global-teleport availability as a spatial/source-side access problem rather than introducing unnecessary persistent search-state dimensions.

### Hub and lifecycle modelling

Investigate remaining issues around shared transport systems such as:

* fairy rings
* spirit trees
* other hub networks

Only add more lifecycle state where benchmarks show it is necessary.

Avoid large combinatorial hub-state masks without evidence.

### Route reconstruction

Improve final path reconstruction so produced routes match useful OSRS/RuneLite movement semantics while preserving efficient search.

## Benchmarking direction

Treat the benchmark corpus as a durable project asset rather than implementation-specific test data.

Longer-term goals:

* versioned benchmark corpus and profile definitions;
* stable expected reachability/reference costs;
* implementation-independent benchmark specification;
* ability to compare different OSRS pathfinding implementations on the same workload;
* controlled distinction between:

  * algorithmic metrics
  * machine-dependent timing metrics;
* eventually make benchmark results publicly browsable.

JSONL should remain a portable canonical result format.

ClickHouse/Grafana are analysis infrastructure, not the benchmark specification itself.

## World-inspection direction

`world-facts.duckdb` should remain a reproducible, disposable inspection artefact derived from the authoritative Haskell world model.

It should make questions such as these easy to answer:

* Which component contains this tile?
* Is this component structurally reachable?
* Which known places are currently unreachable?
* Which locations became reachable after adding a transport?
* Which parts of the world remain unsupported?

Do not move routing semantics into SQL.

## Longer term

### Sailing

Implement Sailing pathfinding and integrate Sailing-only locations into:

* world reachability
* transport modelling
* benchmark corpus
* correctness testing

Previously unreachable sea locations should then become useful coverage signals.

### Public pathfinding site

Eventually expose the pathfinder as a more complete OSRS planning tool.

Users should be able to model their own account state, including:

* inventory
* equipment
* bank contents
* skills
* quests
* diaries
* spellbook
* POH facilities
* permanent unlocks

The web UI should feed the same authoritative account/requirement model used by benchmarks and routing code.

Do not create separate “web UI semantics”.

Potential later integrations include importing account state from RuneLite or another trusted account-data source.

### Integrated benchmark UI

Grafana is the current benchmark-analysis workbench.

If a small set of comparison views proves consistently useful, eventually integrate them into the main viewer/web application.

Likely useful public views:

* implementation/version comparisons
* correctness rate
* benchmark distributions
* per-route performance history
* route-specific comparison
* direct transition from a regression point to spatial route inspection

Do not embed Grafana into the final product merely because it is used during development.

## Project principles

* Correctness before performance.
* Measure broadly before making architectural conclusions.
* Keep the real routing model authoritative in Haskell.
* Keep benchmark definitions implementation-independent where possible.
* Preserve raw benchmark results.
* Treat analysis databases as disposable derived state.
* Keep runtime data structures compact and specialised.
* Use standard query/analysis tools for inspection instead of building bespoke infrastructure unnecessarily.
* Separate structural reachability, account-specific reachability, and benchmark eligibility.
* Prefer simple state models until benchmarks demonstrate the need for additional complexity.
