## Measuring forward-search allocation

Allocation measurement must isolate the **forward A*** itself. Do not report heuristic setup or benchmark harness allocation as forward-search allocation.

### Primary method: per-thread allocation counter

Prefer `System.Mem.getAllocationCounter`.

The allocation counter is per-thread and counts downward as the thread allocates, so:

```haskell
before <- getAllocationCounter

-- force the complete forward-search result here

after <- getAllocationCounter

let allocated = before - after
```

GHC documents this as a lightweight per-thread profiling mechanism; accounting is accurate to roughly 4 KiB.

This is particularly appropriate here because the forward A* is single-threaded and the Cauldron of Thunder → Ice Queen's Lair benchmark allocates enough that the counter's granularity is irrelevant.

Do **not** reset the counter unnecessarily with `setAllocationCounter`; simply sample before and after unless there is a concrete reason to reset it.

### Measure around forcing, not thunk construction

Be careful with laziness.

The measurement must surround the point where the forward-search result is actually forced.

The current structure is approximately:

```haskell
timedIO forceSearch (pure (search ...))
```

so merely sampling around:

```haskell
pure (search ...)
```

would measure almost nothing.

Introduce a dedicated forward-search measurement boundary conceptually like:

```haskell
beforeAlloc <- getAllocationCounter
started <- getMonotonicTimeNSec

let result = search ...
_ <- forceSearch result

finished <- getMonotonicTimeNSec
afterAlloc <- getAllocationCounter

let
  searchMs = ...
  allocatedBytes = beforeAlloc - afterAlloc
```

The heuristic must already have been completely forced before `beforeAlloc`.

This should isolate:

```text
forward A* arrays
PQ
expansion/relaxation
heuristic lookup during forward search
predecessor storage
route reconstruction
```

from:

```text
reverse heuristic construction
seed-table construction
abstract graph preparation
benchmark harness
```

If we want to distinguish route reconstruction later, add another boundary, but do not complicate the initial experiment.

### Record allocation in benchmark results

Add forward-specific metrics such as:

```text
forward_allocated_bytes
forward_allocated_bytes_per_state
```

with:

```text
forward_allocated_bytes_per_state =
    forward_allocated_bytes / states_popped
```

Also derive:

```text
forward_ns_per_state
```

These two metrics should be the primary measures during raw-kernel optimisation:

```text
ns / state popped
bytes / state popped
```

### Secondary method: `GHC.Stats`

For GC-related information, optionally run with:

```text
+RTS -T -RTS
```

and sample `GHC.Stats.getRTSStats` immediately before and after the forced forward search.

`GHC.Stats` exposes cumulative process statistics including:

```text
allocated_bytes
copied_bytes
GC CPU/elapsed statistics
GC counts
```

and requires RTS statistics to be enabled with `-T`.

Take deltas between the two snapshots.

This is less isolated than the per-thread allocation counter, so use:

```text
System.Mem.getAllocationCounter
```

as the authoritative forward-allocation measurement and `GHC.Stats` mainly for understanding whether allocation reductions also reduce GC/copying cost.

### `+RTS -s` sanity check

Also use:

```text
+RTS -s -RTS
```

occasionally as a cheap whole-program cross-check.

It reports total heap allocation, copied bytes, GC time and productivity.

Be careful to pass the RTS flags to the **benchmark executable itself**, not to `cabal`.

Prefer:

```bash
exe=$(cabal list-bin <benchmark-executable>)
"$exe" <benchmark arguments> +RTS -s -RTS
```

rather than relying on ambiguous argument forwarding through `cabal run`.

Remember that `-s` measures the entire executable:

```text
world/query preparation
heuristic setup
forward search
reporting
shutdown
```

so it is a validation tool, not the main measurement for this optimisation project.

## Primary microbenchmark report

For every forward-search optimisation, report the Cauldron of Thunder → Ice Queen's Lair benchmark in this form:

```text
                         Before          After       Change

forward search          X ms            Y ms        ...
states popped           N               N           0
ns/state                 ...             ...         ...
allocated bytes          ...             ...         ...
bytes/state              ...             ...         ...
PQ pushes                ...             ...         ...
walking relaxations      ...             ...         ...
transport relaxations    ...             ...         ...
```

The strongest result is:

```text
same states popped
same route cost
same search semantics

lower ns/state
lower bytes/state
```

Allocation reductions should not be judged by source appearance. Use the allocation counter and generated Core to determine whether a proposed rewrite actually removed runtime allocation.

