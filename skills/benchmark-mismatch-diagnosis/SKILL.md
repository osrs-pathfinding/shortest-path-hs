---
name: benchmark-mismatch-diagnosis
description: Diagnose and, only when explicitly requested, fix one OSRS pathfinding benchmark mismatch by comparing Raw Dijkstra and Tile A*. Use after benchmark-mismatch-analysis has identified a case; do not use for result summaries.
---

# Benchmark mismatch diagnosis

Use only when the user explicitly asks to diagnose or fix a named mismatch in `/home/matt/shortest-path-model`.

## Isolate one case

Use the existing `route-bench` executable and a temporary one-route corpus/oracle. Keep temporary files under `/tmp`; do not rewrite the checked-in corpus or oracle just to isolate a query. Run all four account profiles unless the mismatch is profile-specific.

Establish:

```text
Raw Dijkstra cost == oracle cost
Tile A* cost      == oracle cost
```

If Raw Dijkstra disagrees with the checked-in oracle, classify it as an oracle/corpus problem before changing Tile A*. If Raw and Tile A* agree, classify the saved mismatch as stale.

## Diagnose the root cause

Read callers before editing shared helpers. Compare the raw and Tile A* routes and inspect, in this order:

1. missing or incorrectly filtered real transport edges;
2. blocked transport endpoint/access semantics;
3. bank/global lifecycle and dominance decisions;
4. account-static requirement availability;
5. heuristic pruning or relaxed reverse-distance omissions;
6. only then ordinary walking/component behaviour.

Preserve exact route cost. A heuristic may add optimistic edges but must not omit a real continuation or turn proven relaxed-unreachable states into `h = 0`.

Use search counters and traces to explain the divergence; do not infer a cause from wall time alone.

## Fix only when requested

Make the smallest root-cause change in the shared path used by all relevant solvers. Add one focused regression check when the logic is non-trivial. Then rerun:

1. the focused case across all profiles;
2. the relevant smoke/standard tier;
3. the full correctness pass before declaring the mismatch resolved.

Do not regenerate the oracle unless authoritative raw-routing semantics or corpus/world data changed. Report the mismatch ID, raw/oracle/Tile costs before and after, root cause, verification commands, and remaining mismatches.
