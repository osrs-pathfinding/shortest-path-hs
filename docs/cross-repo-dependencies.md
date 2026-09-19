# Cross-repo dependencies

Updated 2026-09-19 from the current sibling checkouts under `osrs-pathfinding/`.

This file records real repository and artifact edges. A sibling path is a
development default, not a package or release dependency unless stated below.

## Summary

```text
shortest-path                    authoritative RuneLite resources
        │
        ▼
shortest-path-model ──────────── routing model, route-bench, world facts
        │              │  │
        │              │  └── optional runelite-gps-plugin destinations
        │              └───── canonical corpus/profile/oracle fixtures
        ▼
shortest-path-benchmarks         campaign orchestration and analysis

shortest-path-tooling ────────── Java plugin tooling
        ├── shortest-path
        ├── shortest-path-model  (canonical comparison workflows)
        └── shortest-path-corpus

shortest-path-viewer ─────────── interactive model viewer
        ├── shortest-path-model
        └── shortest-path-corpus
```

## Required or normal development edges

| Consumer | Dependency | What crosses the boundary | Default / override | Status |
|---|---|---|---|---|
| `shortest-path-model` | `shortest-path` | Collision map, bank destinations, and all transport TSVs under `src/main/resources/` | `../shortest-path`; override with `SPM_RESOURCES_DIR`, `SPM_COLLISION_ZIP`, and `SPM_BANK_FILE` | Required for loading the real world model. The Haskell library itself does not vendor these resources. |
| `shortest-path-model` | `shortest-path-corpus` | `corpus/routes-v1.json`, `accounts/account-profiles-v1.json`, and `oracle/oracle-v1.json` for `route-bench`, account inspection, and world-facts/corpus tooling | `../shortest-path-corpus`; override with `SHORTEST_PATH_CORPUS_DIR` or an explicit `--corpus-dir` | Required for benchmark/profile workflows; not required for synthetic/core routing when callers provide their own inputs. |
| `shortest-path-benchmarks` | `shortest-path-model` | Cabal package/library, `route-bench`, and JSONL benchmark output | `../shortest-path-model`; override with `SHORTEST_PATH_MODEL_DIR` | Required for Haskell benchmark runs and `bench-import`. |
| `shortest-path-benchmarks` | `shortest-path-corpus` | Canonical routes, profiles, and oracle used by benchmark campaigns and mismatch diagnostics | `../shortest-path-corpus`; override with `SHORTEST_PATH_CORPUS_DIR` | Required for canonical campaigns. |
| `shortest-path-tooling` | `shortest-path` | Java plugin sources, plugin test helpers, and plugin resource files | Git submodule `./shortest-path`; override with `-PshortestPathDir=...` | Required for Java dashboard/benchmark tasks. |
| `shortest-path-tooling` | `shortest-path-corpus` | Canonical routes, neutral account profiles, oracle, and version metadata | `../shortest-path-corpus`; override with `-PcorpusDir=...` or `SHORTEST_PATH_CORPUS_DIR` | Required only for canonical Java-vs-Haskell tasks; legacy dashboard/benchmark tasks use local datasets. |
| `shortest-path-viewer` | `shortest-path-model` | Haskell library, viewer backend, model resources, and generated `out/world-facts.duckdb` | `../shortest-path-model`; override with `SHORTEST_PATH_MODEL_DIR` and `WORLD_FACTS_DB` | Required for the backend and endpoint refinement. |
| `shortest-path-viewer` | `shortest-path-corpus` | Canonical route JSON served by the viewer and used for endpoint-refinement display | `../shortest-path-corpus`; override with `SHORTEST_PATH_CORPUS_DIR` | Required for corpus-backed viewer features; the viewer can start without optional debug fixtures. |

## Workflow-only and optional edges

### `shortest-path-model` -> `runelite-gps-plugin`

The model's `world-facts` executable and corpus-refinement scripts can read
`src/main/resources/destinations.tsv` from the separate GPS plugin repository.
The default is `../runelite-gps-plugin`; use `WORLD_FACTS_GPS_DESTINATIONS` or
`GPS_PLUGIN_DIR` to point elsewhere.

This is optional: `world-facts` treats a missing GPS destinations file as an
empty set, and the core router does not need that repository.

### `shortest-path-benchmarks` -> `shortest-path-viewer`

The benchmark repository's unreachable-route diagnostic document uses the
viewer for visual inspection. This is a documented/manual workflow, not a
Cabal or runtime dependency.

### Derived artifact hand-offs

These are file contracts rather than library dependencies:

| Producer | Artifact | Consumer |
|---|---|---|
| `shortest-path-model` `world-facts` | `out/world-facts.duckdb` | Model corpus-maintenance scripts and `shortest-path-viewer` endpoint inspection |
| `shortest-path-model` `route-bench` | `out/route-benchmark.jsonl` | `shortest-path-benchmarks` import/analysis and optionally `shortest-path-viewer` |
| `shortest-path-tooling` canonical benchmark tasks | Java canonical JSONL | Tooling-local dashboards and comparison scripts |
| `shortest-path-model` separator generation | `data/routing-separators-v1.json` | Model topology construction; generated from `shortest-path` resources but checked in here |

The corpus oracle is another deliberate hand-off: `shortest-path-model` writes
`shortest-path-corpus/oracle/oracle-v1.json`, while corpus validation owns the
schema and cross-file checks.

## Known stale or broken edges

1. `shortest-path-tooling/scripts/compare-canonical-route.sh` still searches
   for a `pathfinder-tool` executable and sends it a `serve` protocol. The
   current `shortest-path-model` Cabal file has no `pathfinder-tool` target or
   `serve` executable; its current executable surface is `route-bench`,
   `routing-artifact`, `separator-artifact`, `world-facts`, and
   `account-profile`. The canonical comparison script needs a future adapter
   update before it can be considered a live dependency.

2. The model's GPS-plugin path is intentionally optional but points at a
   repository not present in this workspace. Keep it out of core build/test
   requirements unless GPS destinations become part of the canonical model.

## Deliberate non-dependencies

- The production `shortest-path` RuneLite plugin does not depend on
  `shortest-path-model` or `shortest-path-corpus` at build/runtime.
- `shortest-path-corpus` is implementation-neutral data; it does not import
  Haskell or Java code. Its oracle is currently generated by the model as a
  workflow, not by a package dependency.
- `shortest-path-benchmarks` does not own routing semantics; `route-bench`
  remains in `shortest-path-model`.
- The model-owned separator JSON is not an upstream plugin artifact. It is a
  checked-in derived input owned and validated by the model repository.
