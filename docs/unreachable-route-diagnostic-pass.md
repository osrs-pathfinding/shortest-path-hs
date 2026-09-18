# Unreachable route diagnostic pass

## Goal

Classify every route/profile result currently reported as unreachable without changing routing code or transport data.

Use these broad classifications:

1. `profile_requirement` — the implemented graph has a route when requirements are ignored, but the named profile does not.
2. `topology_or_endpoint` — the route is unreachable even when requirements are ignored.
3. `tile_astar_correctness` — Reference Dijkstra can reach the target with the same settings, but Tile A* cannot.
4. `expected_unreachable` — the route is intentionally unreachable, such as an explicit negative regression or a disabled movement system.
5. `needs_manual_review` — evidence is insufficient or contradictory.

Do not fix anything during this pass. Record evidence and continue.

## Inputs

- Corpus: `../shortest-path-corpus/corpus/routes-v1.json`
- Canonical profiles: `early`, `mid`, `end`, `maxed`
- Regenerated Tile A* results: `out/oracle-unreachable-tile-astar-2026-09-08.jsonl`
- Requirements-ignored results for routes unreachable by all four profiles: `out/all-profile-unreachable-everything-enabled-summary-2026-09-08.jsonl`
- Human notes: `out/unreachable-route-investigation-2026-09-08.md`

In this document, **everything enabled** means a normal Tile A* query with no `accountProfile`. This selects `IgnoreRequirements`. It does not create a fifth account profile, and it still excludes transport types disabled by `defaultQuery`, such as seasonal transports.

## Pass 1: build the reachability matrix

Group JSONL rows by `routeId`. For each route, record:

```text
early  mid  end  maxed  everything
```

Ignore benchmark `correct` when deciding current reachability; use the freshly measured `reachable` field. `correct` only compares the result with the old oracle.

Process routes in this order:

1. All four named profiles unreachable and everything unreachable.
2. All four named profiles unreachable and everything reachable.
3. Some named profiles reachable.

The first group is the highest-value manual topology queue. The other groups are usually suitable for automated requirement analysis.

## Pass 2: classify from everything-enabled reachability

### Everything enabled is reachable

Assign provisional classification `profile_requirement`.

Run the route with `finder: "tile-full"` once for everything enabled and once for the nearest successful named profile, if one exists. Preserve the returned path and extract every `kind: "transport"` step in order.

For every failing profile, evaluate those transports with the existing authoritative requirement path:

```text
transportExplanation query banked transport
```

Record the exact returned failures, using the existing categories:

- `MissingItems`
- `MissingSkills`
- `MissingQuests`
- `FailedVarRequirements`
- `UnknownVarRequirements`
- `MissingCapability`
- `TransportTypeDisabled`

Do not infer the blocker from a transport label alone. A successful everything-enabled path proves that implemented topology exists; it does not prove that every transport on that particular path is necessary. Report blockers as `candidate` unless disabling them or using the authoritative evaluator proves the route/profile distinction.

Preserve item and lifecycle semantics. Evaluate carried and bank-accessible contexts separately, and do not treat an unavailable bank-global opportunity as the `Banked` state.

Useful supporting commands:

```sh
nix-shell --run 'cabal run account-profile -- validate early'
nix-shell --run 'cabal run account-profile -- compare early mid'
nix-shell --run 'cabal run account-profile -- vars'
```

These commands provide profile-wide evidence only. They do not prove the blocker for one route.

### Everything enabled is unreachable

Assign provisional classification `topology_or_endpoint`, then run the identical query with `finder: "raw"` and requirements still ignored.

- Raw reachable, Tile A* unreachable: classify `tile_astar_correctness`. Record both costs and stop; this requires a synthetic correctness regression before any fix.
- Raw unreachable, Tile A* unreachable: retain `topology_or_endpoint`.

For `topology_or_endpoint`, inspect in this order:

1. Is `allowTransports` false for the corpus route?
2. Is either endpoint unresolved or attached to an unexpected natural component?
3. Is the required directed transport absent from the loaded world?
4. Is the transport present but attached through incorrect blocked-endpoint semantics?
5. Is the destination part of an intentionally unsupported movement system, especially Sailing?
6. Is directionality the reason the reverse route works while this route does not?

Use Haskell-produced world facts rather than recreating collision or endpoint attachment logic:

```sh
nix-shell --run 'cabal run world-facts'
duckdb data/world-facts.duckdb
```

Relevant tables/views are `point_access`, `place_facts`, and `transport_facts`. Treat the database as derived evidence, not an authority separate from the Haskell preprocessing.

## Direct route requests

Use the existing direct server. Omitting `accountProfile` means everything enabled; adding `"accountProfile":"maxed"` selects a named profile.

Do not start the server once per route. `serve` loads the world and Tile A* cache once, then accepts newline-delimited JSON requests until stdin closes. Generate a batch request file with a unique `id` for every route/profile pair and process it in one invocation:

```sh
nix-shell --run 'cabal run pathfinder-tool -- serve \
  < out/unreachable-diagnostic-requests.jsonl \
  > out/unreachable-diagnostic-responses.jsonl'
```

The output also contains startup messages and one `{"ready":true}` record. When parsing, retain JSON objects with a request `id` and join them back to the request file by that ID.

Use at most two server starts for the diagnostic pass:

1. One batched Tile A* run containing every required named-profile and everything-enabled query.
2. One batched Reference Dijkstra run containing only the everything-enabled cases that Tile A* could not reach.

Do not run Reference Dijkstra speculatively for every case; some routes are expensive and it is only needed to distinguish topology failures from Tile A* correctness failures.

```sh
printf '%s\n' '{
  "id":1,
  "start":{"x":1260,"y":3674,"plane":0},
  "target":{"x":2924,"y":5811,"plane":0},
  "allowTransports":true,
  "includeExpandedTiles":false,
  "useHeuristic":true,
  "finder":"tile-full"
}' | nix-shell --run 'cabal run pathfinder-tool -- serve'
```

For unreachable direct-server responses, `cost` is currently encoded as Haskell `maxBound` (approximately `9.22e18`), not `null`. Treat `cost >= 9e18` as unreachable when parsing this diagnostic output.

## Required output per route

Append one row to the investigation ledger with:

```text
route ID
route name
unreachable named profiles
everything reachable: yes/no
Reference Dijkstra reachable: yes/no/not run
classification
successful path transport labels
exact or candidate failed requirements
endpoint/component evidence
short diagnosis
confidence: confirmed/likely/unknown
```

Keep diagnoses factual and short. Good examples:

```text
profile_requirement — everything and mid reach via Transport X; early receives MissingQuests [Q] for X; confirmed.
topology_or_endpoint — both solvers fail with requirements ignored; target resolves to component N with no incoming implemented transport; likely missing transport.
tile_astar_correctness — Raw cost 42, Tile A* unreachable under identical query; confirmed correctness bug.
```

## Guardrails

- Never rewrite the oracle during diagnosis.
- Never change a profile merely to make a route reachable.
- Never classify a missing game variable as value zero.
- Never weaken unreachable heuristic pruning to make a case pass.
- Keep real route cases as integration evidence, but propose the smallest synthetic graph for any Tile A* correctness bug.
- If evidence does not distinguish missing requirements from missing topology, use `needs_manual_review`.
