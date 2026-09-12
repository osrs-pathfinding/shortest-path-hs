---
name: benchmark-mismatch-analysis
description: Read OSRS pathfinding benchmark results, summarize correctness mismatches, and identify the first case for follow-up. Use after route-bench runs; do not diagnose, edit code, or analyze performance.
---

# Benchmark mismatch analysis

Use this read-only workflow for correctness results in `/home/matt/shortest-path-model`. Stop after reporting the results and the first mismatch.

## Establish the result set

1. Inspect `git status`, the current commit, and the benchmark corpus/oracle paths.
2. Run the smallest fresh correctness pass appropriate to the request. For the complete corpus:

   ```sh
   nix-shell --run 'cabal run route-bench -- --tier full --runs 1 --output /tmp/full-benchmark.jsonl'
   ```

   `--runs 1` is sufficient for correctness. Do not add `--diagnostic` to the full pass: it reruns Raw Dijkstra for every query.
3. Extract only incorrect rows from the fresh JSONL. Prefer `jq`:

   ```sh
   jq -c 'select(.correct == false)' /tmp/full-benchmark.jsonl
   ```

   If there are no incorrect rows, say so and treat older mismatch fixtures as stale until proven otherwise.

## Select the first case

Order mismatches deterministically by corpus order, then profile. Start with the first mismatch and record:

- stable route ID and account profile;
- start, target, transport setting, and expected cost/reachability;
- actual Tile A* cost/reachability;
- commit, corpus, oracle, and world-data identity.

Do not investigate several mismatches, compare routes, change the algorithm, regenerate the oracle, or edit files. Hand the first mismatch to the separate diagnosis skill when the user explicitly asks for that work.

## Report

Report:

- result file and timestamp;
- total, correct, and mismatched rows;
- mismatch counts by category and account profile;
- the first mismatch's route ID, profile, coordinates, actual cost/reachability, and expected cost/reachability;
- commit, dirty state, corpus, and oracle paths.

Do not infer whether the oracle or Tile A* is wrong from this read-only pass.
