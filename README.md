# shortest-path-model

## Tile A* Experiment Handoff

Build the benchmark executable:

```sh
nix-shell --run 'cabal build exe:hierarchy-differential'
```

Warm or rebuild the tile-A* static cache, including natural walking components and the sparse Manhattan walking network:

```sh
dist-newstyle/build/x86_64-linux/ghc-9.10.3/shortest-path-model-0.1.0.0/x/hierarchy-differential/opt/build/hierarchy-differential/hierarchy-differential tile-static-report
```

Run correctness checks:

```sh
nix-shell --run 'cabal test tile-astar'
nix-shell --run 'cabal test hierarchy-synthetic'
nix-shell --run 'SPM_TILE_REVERSE_IMPL=manhattan cabal test hierarchy-synthetic'
```

Run the Kourend -> Desert benchmark through the direct server protocol:

```sh
printf '%s\n' '{"id":1,"start":{"x":1503,"y":3553,"plane":0},"target":{"x":3359,"y":2912,"plane":0},"allowTransports":true,"includeExpandedTiles":false,"useHeuristic":true,"finder":"tile-full"}' \
  | dist-newstyle/build/x86_64-linux/ghc-9.10.3/shortest-path-model-0.1.0.0/x/hierarchy-differential/opt/build/hierarchy-differential/hierarchy-differential serve-direct
```

Compare reverse-heuristic implementations:

```sh
# Reference implicit same-component Chebyshev clique.
printf '%s\n' '{"id":1,"start":{"x":1503,"y":3553,"plane":0},"target":{"x":3359,"y":2912,"plane":0},"allowTransports":true,"includeExpandedTiles":false,"useHeuristic":true,"finder":"tile-full"}' \
  | dist-newstyle/build/x86_64-linux/ghc-9.10.3/shortest-path-model-0.1.0.0/x/hierarchy-differential/opt/build/hierarchy-differential/hierarchy-differential serve-direct

# Sparse Manhattan walking network.
printf '%s\n' '{"id":1,"start":{"x":1503,"y":3553,"plane":0},"target":{"x":3359,"y":2912,"plane":0},"allowTransports":true,"includeExpandedTiles":false,"useHeuristic":true,"finder":"tile-full"}' \
  | SPM_TILE_REVERSE_IMPL=manhattan dist-newstyle/build/x86_64-linux/ghc-9.10.3/shortest-path-model-0.1.0.0/x/hierarchy-differential/opt/build/hierarchy-differential/hierarchy-differential serve-direct
```

Check sparse Manhattan reverse labels against the clique reference for the query:

```sh
printf '%s\n' '{"id":1,"start":{"x":1503,"y":3553,"plane":0},"target":{"x":3359,"y":2912,"plane":0},"allowTransports":true,"includeExpandedTiles":false,"useHeuristic":true,"finder":"tile-full"}' \
  | SPM_TILE_REVERSE_IMPL=manhattan SPM_TILE_COMPARE_REVERSE=1 dist-newstyle/build/x86_64-linux/ghc-9.10.3/shortest-path-model-0.1.0.0/x/hierarchy-differential/opt/build/hierarchy-differential/hierarchy-differential serve-direct
```

Optional diagnostic counters:

```sh
SPM_TILE_REVERSE_COUNTERS=1
```

Do not use counter-enabled timings as wall-clock benchmark results; reverse counter threading materially slows the hot loop.

Current useful flags:

```text
SPM_TILE_REVERSE_IMPL=manhattan   use sparse Manhattan reverse Dijkstra
SPM_TILE_COMPARE_REVERSE=1        compare sparse labels against clique labels
SPM_TILE_REVERSE_COUNTERS=1       enable expensive reverse diagnostics
SPM_HEURISTIC_TRANSFORM=c         use C Chebyshev transform for heuristic image rendering
```

Recent laptop baseline for Kourend -> Desert, 5 warm in-process runs:

```text
Clique median:
  total:   382 ms
  setup:   252 ms
  reverse: 247 ms
  search:  121 ms

Sparse Manhattan CSR median:
  total:   316 ms
  setup:   189 ms
  reverse: 178 ms
  search:  140 ms
```

Static sparse graph size from `tile-static-report`:

```text
static original sites: 7349
static Steiner vertices: 37194
static total vertices: 44543
sparse walking undirected edges: 68667
rough adjacency memory estimate: 2.07 MiB
```
