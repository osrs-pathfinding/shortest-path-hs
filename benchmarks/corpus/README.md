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

Keep both corpus files pretty-printed for readable diffs. After regenerating
either file, run this from `shortest-path-model`:

```sh
nix-shell --run 'for file in benchmarks/corpus/oracle-v1.json benchmarks/corpus/routes-v1.json; do tmp="$file.tmp"; jq --indent 2 . "$file" > "$tmp" && mv "$tmp" "$file"; done'
```

`sentinels-v1.json` contains the individually tracked route/profile pairs.

`account-profiles-v1.json` is a language-neutral serialization of the four
compiled benchmark `AccountState`s used for cross-language benchmark parity.
Regenerate it with:

```bash
cabal run account-profile -- export-java benchmarks/corpus/account-profiles-v1.json
```

Before generating the oracle, run `node benchmarks/validate-corpus.js`. It
requires IDs, raw/resolved route coordinates, names, provenance, tier tags,
valid classifications, a matching oracle, and disjoint exclusions.
