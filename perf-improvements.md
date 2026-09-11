# Forward A* raw performance optimisation plan

Goal: push the raw throughput of the **forward tile A*** search as far as reasonably possible without changing the algorithm, heuristic values, routing semantics, or search ordering.

This task is exclusively about:

> How cheaply can the existing Haskell implementation expand one A* state?

Do not work on reverse search, Manhattan construction, seed generation, target attachment, caching, account compilation, or heuristic-strength improvements.

## Primary microbenchmark

Use:

```text
Cauldron of Thunder -> Ice Queen's Lair
```

as the primary optimisation benchmark.

Locate the existing route in the benchmark corpus and use its exact coordinates/query configuration rather than creating a new approximation.

Use **exact A***:

```text
heuristic weight = 1
```

for this performance work.

This route is deliberately useful because it produces a very large forward search. That makes fixed overhead and measurement noise insignificant and gives a strong signal for improvements in:

* allocation per expansion;
* heap operations;
* neighbour generation;
* relaxation;
* heuristic lookup;
* array access;
* state representation.

Do not try to make this route expand fewer states during this task.

For raw-performance comparisons, its `states_popped` should remain identical unless an implementation detail unexpectedly changes ordering. If it changes, investigate before accepting the result.

## Baseline context

The current full-suite baseline is:

```text
manhattan-full-20260911T130038Z-e29273e
```

Across 3,000 per-case medians:

| Percentile | Forward search | States explored |
| ---------- | -------------: | --------------: |
| P50        |        28.7 ms |           1,415 |
| P80        |        88.6 ms |           8,682 |
| P90        |       277.6 ms |          28,672 |
| P95        |       971.9 ms |         112,132 |
| P99        |       13.637 s |       1,348,760 |
| Max        |        ~23.4 s |       2,484,932 |

`expanded_nodes == states_popped` across all 3,000 cases.

Ice Queen's Lair is one of the principal pathological destination clusters and therefore provides an excellent stress test for raw forward-search throughput.

## Primary measurements

For every optimisation, benchmark the Cauldron of Thunder → Ice Queen's Lair case before and after.

Record:

```text
forward_search_ms
states_popped
unique_states_reached
PQ pushes
stale PQ entries
walking relaxations
transport relaxations
```

Most importantly derive:

```text
ns / state_popped
```

Where practical also record:

```text
allocated_bytes
allocated_bytes / state_popped
GC time
copied bytes
```

The main success criterion is:

```text
same route result
same route cost
same states_popped

lower forward_search_ms
lower ns/state
lower allocation/state
```

## Benchmark workflow

During development, **do not run the 3,000-case suite after every edit**.

Use this workflow:

### Inner optimisation loop

Run only:

```text
Cauldron of Thunder -> Ice Queen's Lair
```

Prefer enough repetitions to obtain a stable median.

Use this to make quick decisions about whether a change genuinely improves forward-search throughput.

For small differences, repeat sufficiently to distinguish signal from noise.

### Before keeping a change

Once the microbenchmark shows a convincing improvement:

1. run relevant correctness/unit tests;
2. run a small representative route set if useful;
3. run the complete 3,000-case benchmark;
4. verify no correctness regression;
5. inspect P50/P80/P90/P95/P99 forward-search changes;
6. commit the optimisation separately.

The microbenchmark tells us whether the kernel got faster.

The 3,000-case suite tells us whether the optimisation generalises.

## Important distinction

Do not optimise the pathological **number of expansions** in this task.

We already know that improving the heuristic or using weighted A* can reduce this route from millions of states to tens of thousands. That is a separate algorithmic performance project.

Here, the millions of states are useful.

They effectively provide a long-running workload for answering:

> Given that A* must pop this many states, how fast can the Haskell implementation do it?

Therefore do not:

* change heuristic weight;
* strengthen the heuristic;
* change tie-breaking intentionally;
* add new dominance;
* prune additional states;
* change graph semantics.

If a change reduces `states_popped`, do not count the resulting wall-time improvement as evidence that the raw implementation became faster.

## Optimisation order

Work incrementally in this order.

### 1. Establish allocation and throughput baseline

For the Cauldron → Ice Queen route record:

```text
forward_search_ms
states_popped
ns/state
allocated bytes
bytes/state
GC time
```

Inspect optimised Core/STG for the forward search.

### 2. Eliminate neighbour-list construction

Replace construction of:

```haskell
walk <> bank <> localTransports <> bankGlobalTransports
```

and tuples such as:

```haskell
(next, cost, RouteStep, EdgeKind)
```

with direct imperative relaxation loops.

Benchmark immediately.

### 3. Remove rich `RouteStep` values from exploration

Store primitive predecessor metadata.

Construct `Walk` / `UseTransport` objects only during final route reconstruction.

Benchmark independently.

### 4. Reduce large search-array initialisation

Experiment with primitive/uninitialised arrays plus validity tracking rather than filling the entire state space on every search.

Benchmark independently.

### 5. Eliminate repeated tile/node/component lookup

Once a state has a node ID, use node-indexed arrays rather than converting back to tiles and binary-searching to recover static information.

Benchmark independently.

### 6. Optimise walking expansion

Investigate compact walking masks or precomputed primitive neighbour IDs.

Walking is sufficiently frequent that small per-edge savings may matter considerably on this benchmark.

### 7. Flatten prepared transport edges

Move Maps, Strings, rich `Transport` records, `Maybe destination`, and penalty lookup out of the forward hot path.

Consume compact prepared node-indexed edges instead.

### 8. Optimise forward heuristic lookup

The construction of the heuristic is out of scope.

The cost of querying the already-built heuristic during every state relaxation is in scope.

Inspect generated code before rewriting vector operations.

### 9. Inspect PQ representation

Only optimise heap implementation details demonstrated to survive into the hot path.

Do not change A* queue semantics.

### 10. Measure instrumentation overhead

Compare the normal counters against a controlled minimally-instrumented search.

Only redesign counters if the difference is material.

### 11. Inspect final Core/STG

Look for remaining hot-loop allocation or abstraction overhead:

```text
(:)
Just
tuples
State
Tile
RouteStep
counter records
closures
Map lookup
boxed data
```

Also inspect repeated binary searches, bounds checks and failed inlining.

## Desired final result

The forward search should approach the shape of an imperative C/Rust/Java graph-search kernel:

```text
pop primitive state

read primitive state/node data

lookup cached heuristic value/data

relax walking edges directly

relax bank transition if applicable

relax prepared transport edges directly

write primitive best/predecessor state

repeat
```

The final hot path should contain as little allocation and indirection as practical.

The headline measurement for this exercise is:

```text
Cauldron of Thunder -> Ice Queen's Lair
forward search:
    before: X ms / Y states
    after:  Z ms / Y states

ns/state:
    before: ...
    after:  ...

bytes/state:
    before: ...
    after:  ...
```

Then use the full 3,000-case run to verify that the same improvement carries across the workload.


