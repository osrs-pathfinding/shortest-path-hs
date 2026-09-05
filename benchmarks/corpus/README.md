# Route corpus placeholder

`routes-v1.json` intentionally has no selected routes yet. Populate it with the
better-model-selected, fixed route corpus; each route must provide a stable `id`.
Use a `tiers` array to place its stable ID in the fixed `smoke`, `standard`, and
`full` subsets. The runner uses all routes for `full`; it filters the other two
with `--tier smoke` or `--tier standard`.

Until then, `route-bench --seed` uses `../routes.json` only to prove the runner,
profiles, oracle generation, and JSONL output work end to end. Seed results are
not the v1 benchmark corpus.

`sentinels-v1.json` is also intentionally empty. After selecting stable sentinel
route/profile pairs, add objects such as `{"routeId":"quest-natural-0017","accountProfile":"mid"}`.

Before generating the oracle, run `node benchmarks/validate-corpus.js`. It
requires IDs, raw/resolved route coordinates, names, provenance, and tier tags.
