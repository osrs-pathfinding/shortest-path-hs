# Route corpus v1

`routes-v1.json` is the fixed benchmark population; each route provides a stable `id`.
Use a `tiers` array to place its stable ID in the fixed `smoke`, `standard`, and
`full` subsets. The runner uses all routes for `full`; it filters the other two
with `--tier smoke` or `--tier standard`.

Profile-specific expected-unreachable cases are listed in each route's
`negativeProfiles`; every other route/profile pair is positive. The 26 routes
in `excluded-routes-v1.json` are unresolved world-model gaps and are not run.
`oracle-v1.json` records the exact Dijkstra reachability and cost for every
included route/profile pair.

`sentinels-v1.json` contains the individually tracked route/profile pairs.

Before generating the oracle, run `node benchmarks/validate-corpus.js`. It
requires IDs, raw/resolved route coordinates, names, provenance, tier tags,
valid classifications, a matching oracle, and disjoint exclusions.
