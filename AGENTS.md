# AGENTS.md

This repository is an experimental/production-oriented OSRS pathfinding implementation. The main focus is **exact or near-exact tile pathfinding with transports and strong transport-aware heuristics**, backed by a broad reproducible benchmark corpus.

## Project map

Key Haskell modules:

* `src/ShortestPath/Exact/TileAStar.hs` — current primary tile A* implementation and heuristic/lifecycle logic.
* `src/ShortestPath/Exact/ReferenceDijkstra.hs` — deliberately simple correctness/reference solver.
* `src/ShortestPath/World.hs` — collision-backed walkability and authoritative walking neighbours.
* `src/ShortestPath/Topology.hs` — natural components, point attachments and structural reachability.
* `src/ShortestPath/Transport.hs` — transport definitions and parsing.
* Hierarchical routing is historical/future experimental work, not maintained runtime architecture.

Other important areas:

* benchmark runner/corpus — fixed route corpus, account profiles, correctness oracle and JSONL performance output.
* viewer — spatial inspection of individual routes/search behaviour.
* world inspection data — derived/queryable facts about components, places and structural reachability.
* benchmark analysis — JSONL is canonical; databases/Grafana are derived analysis infrastructure.

Planning and algorithm references:

* [ROADMAP.md](ROADMAP.md) — project priorities and longer-term direction.
* [ALGORITHM.md](ALGORITHM.md) — current routing algorithm and design details.

Before making substantial changes, locate the current benchmark/profile/corpus definitions rather than relying on old experimental modules.

## Core routing model

Walking:

```text
8-directional
every legal walking step costs 1
diagonals also cost 1
```

Therefore relaxed same-component geometric distance is Chebyshev distance.

Movement between disconnected natural components requires an explicit transport.

Transports are directed and weighted and may depend on skills, quests, items, diaries, vars, account state, etc.

## Important correctness invariants

### Heuristic

The heuristic is a relaxation of the real routing graph.

It may add optimistic possibilities, but **must never omit a real continuation**. This is particularly important because relaxed-unreachable states are pruned entirely.

For a tile in a natural component, the transport-aware heuristic is conceptually:

```text
min over reachable transport sites s:
    Chebyshev(tile, s) + relaxedDistanceToTarget(s)
```

Do not turn missing heuristic information into `h = 0` when the relaxed model proves the state unreachable; prune it.

### Endpoint/component semantics

A transport site may itself be blocked but accessible from adjacent walkable tiles. Fairy rings exposed this bug previously.

`NaturalComponentId` semantics are ordinary walking connectivity only; IDs do
not encode structural reachability. A routing point may attach to zero, one, or
multiple components. Use `pointAttachments` from `ShortestPath.Topology`; never
select the first adjacent component or reconstruct attachment adjacency in a
consumer.

Structural reachability is a separate derived property with an explicit seed
policy. Account-specific reachability and benchmark eligibility remain above
that world layer. A missing production seed is an error, not permission to mark
every component reachable.

`world-facts` is derived from this same topology. Tile A* may relax it, but the
heuristic and unreachable pruning must preserve every valid attachment.

### Bank/global lifecycle

Do not collapse the three relevant phases:

```text
UnbankedGlobalAvailable
UnbankedGlobalUnavailable
Banked
```

“Cannot use the bank-global opportunity” does **not** imply “banked items are available”.

Initial source-only globals should be resolved before ordinary heuristic evaluation.

### Requirements

Static account facts are not A* state dimensions.

Evaluate/prefilter account-static transport availability before the hot search loop. Reference Dijkstra, Tile A*, and the heuristic must agree on real transport availability.

Keep:

```text
carried items
bank-accessible items
bank/global lifecycle
```

as separate concepts.

## Building and testing

Use the repository's existing Cabal targets and scripts. Inspect the `.cabal`/project files for executable names rather than inventing new wrappers.

For algorithm changes:

1. build the affected targets;
2. run focused unit/regression tests;
3. run the smoke benchmark corpus;
4. check route cost/correctness before considering performance;
5. use the larger benchmark tiers before drawing architectural conclusions.

Do not optimise from one hand-picked route.

## Changing the core algorithm

Start in:

```text
src/ShortestPath/Exact/TileAStar.hs
```

Then inspect the corresponding world/transport helpers before duplicating logic.

Measure at least:

```text
total/query time
heuristic setup time
reverse heuristic time
search time
states expanded/popped
PQ activity
walking relaxations
transport relaxations
```

Expansion counts are useful for distinguishing algorithmic improvements from timing noise.

Preserve exact/reference path cost unless intentionally working on weighted/non-exact search.

## Adding or changing transports

1. update transport parsing/data;
2. update the authoritative requirement evaluator;
3. ensure Reference Dijkstra and Tile A* expose the same real edge;
4. ensure the relaxed heuristic still contains every real edge or an optimistic equivalent;
5. add targeted tests;
6. benchmark representative account profiles.

Avoid strings/maps/requirement-expression evaluation in the search hot loop; prepare compact eligible transport structures first.

## Benchmark corpus

The benchmark corpus is intended to become an implementation-independent OSRS pathfinding benchmark, not just test data for this algorithm.

A full run is roughly:

```text
750 routes × 4 profiles ≈ 3000 cases
```

Profiles are approximately:

```text
early
mid
end
maxed
```

Keep route IDs and profile semantics stable once a corpus version is published.

JSONL benchmark results are canonical. ClickHouse/Grafana or other databases are disposable derived state.

### Corpus endpoint eligibility

Keep the broad destination/place catalogue even when locations are currently unreachable.

Ordinary performance routes should use endpoints that are **structurally reachable using movement systems currently implemented by the pathfinder**.

In particular, Sailing sea/ocean locations should currently remain in the place catalogue but should not be ordinary benchmark endpoints until Sailing routing exists.

Do not filter by names such as `Sea` or `Ocean`; use authoritative component/reachability facts.

Profile-specific unreachability is different and may be a valid benchmark result.

Preserve explicit unreachable regression cases separately.

## World inspection / reachability facts

The authoritative world/component semantics belong in Haskell.

A derived `world-facts.duckdb` may expose those already-computed facts for agents, scripts and corpus analysis.

Preferred boundary:

```text
authoritative Haskell preprocessing
        ├── compact runtime structures -> pathfinder
        └── exported facts -> DuckDB -> inspection/analysis
```

Do not recreate collision, endpoint resolution, blocked-origin handling or reachability semantics in SQL.

Useful facts include:

```text
tile -> natural component
component -> structurally reachable
places/destinations
resolved endpoint access
provenance/world-data version
```

DuckDB should be disposable and reproducibly regenerated.

## Benchmark analysis

When comparing implementations or commits:

1. correctness first;
2. compare only compatible corpus/profile definitions;
3. compare wall time only on compatible testbeds;
4. inspect distributions and individual cases, not only averages.

Useful views include:

```text
candidate vs baseline scatter
ranked regressions
change distribution
quantile/CDF
per-case metric comparison
historical case performance
```

Use Grafana/ClickHouse for broad comparison and the pathfinding viewer to understand **why** one particular route behaved badly.

## Synthetic regression cases

When an algorithmic edge case is discovered, prefer adding a **small synthetic regression case that isolates the underlying graph property**, rather than relying only on the real-world route that exposed it.

Real OSRS routes are useful integration tests, but they often involve many unrelated transports, collision details, requirements, banking states, and heuristic interactions. A synthetic case should reduce the failure to the smallest graph that still exhibits the behaviour.

This is especially important for correctness-sensitive interactions between:

* exact search semantics and heuristic abstractions;
* blocked transport origins or destinations;
* transport-only intermediate states;
* states with no natural walking component;
* banking/capability state transitions;
* directed or asymmetric transports;
* unreachable states and heuristic pruning;
* multiple components or attachment choices;
* dominance rules and state-space reductions.

When fixing such a bug:

1. First identify the **abstract property that was violated**. Do not make the regression test merely reproduce particular OSRS coordinates.
2. Construct the smallest synthetic world that exhibits that property.
3. Where useful, compare the optimized algorithm against Reference Dijkstra as the correctness oracle.
4. Make the synthetic test fail before the fix and pass after it.
5. Keep the original real-world case as an integration regression when practical.

Synthetic tests should test both the positive case and nearby negative cases when that distinction is important.

For example, if a bug involves a blocked transport-only state with no walking component, a strong test is:

```text
walkable A
    |
    | transport
    v
blocked X
    |
    | transport
    v
walkable B
```

`X` should have no natural walking component but should remain a valid explicit graph state because it is a transport destination and origin. A companion dead-end transport-only state can verify that genuinely unreachable states are still pruned.

This is preferable to weakening an invariant globally just to make the observed route succeed. For example, a fix that turns every unknown heuristic value into `h = 0` may make one route work while masking the real distinction between:

```text
explicit transport site with a finite relaxed distance
```

and:

```text
state that is genuinely unreachable in the relaxed graph
```

The synthetic suite should encode these distinctions explicitly.

When adding a new algorithmic mechanism or fixing a subtle correctness issue, ask:

> What is the smallest synthetic graph that would have caught this bug before it reached the real world?

If that graph is reasonably small to express, add it to the synthetic suite.


## Before finishing a change

Check:

* Have real routing edges accidentally disappeared from the heuristic relaxation?
* Are unreachable states being classified soundly?
* Are blocked transport endpoints handled using real access semantics?
* Have bank/global lifecycle states been conflated?
* Did account requirement changes affect all solvers consistently?
* Is the benchmark result still correct, not merely faster?
* Did the change help broadly across the corpus rather than only one route?
* If world/corpus semantics changed, should the corpus/world-facts identity change?

Prefer measuring over guessing.

## Game-state profiles

The account pipeline is:

```text
AccountSpec -> compileAccount -> AccountState -> RequirementContext
            -> transportAvailability -> prepared transports -> pathfinder
```

`ShortestPath.AccountSemantics` owns semantic OSRS types and numeric game-state
derivation. `ShortestPath.BenchmarkProfiles` owns only benchmark fixture
definitions. `ShortestPath.Account.transportAvailability` is the authoritative
requirement evaluator used through `prepareQueryTransports`; routing algorithms
must not independently interpret account facts. Raw varbit/varplayer overrides
are exceptional and contradictory derived/raw values must be rejected.

RuneLite varbit and varplayer IDs are numeric at the external GPS TSV boundary, but human-authored account/profile logic must use semantic game-variable names from the generated `ShortestPath.GameVars` modules. Do not add unexplained numeric varbit/varplayer IDs to benchmark profiles. Resolve new transport-relevant IDs through RuneLite's generated `gameval/VarbitID.java` or `gameval/VarPlayerID.java`, regenerate the semantic mapping, classify the variable as progression, permanent unlock, configuration or runtime state, and add a regression test for newly discovered routing semantics.

A missing profile variable is not equivalent to value zero. New transport requirements must either be modelled or explicitly classified before relying on benchmark results involving that transport.
