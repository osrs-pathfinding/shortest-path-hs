# KaHIP Door Experiments

This guide assumes:

- regenerated collision data is under `../shortest-path/src/main/resources`
- filtered door transports are at `../shortest-path-tooling/door_transports.tsv`
- `metis-partition` is run from this repository

## Common Environment

Use these env vars for all door-chart runs:

```bash
export SPM_RESOURCES_DIR=../shortest-path/src/main/resources
export SPM_COLLISION_ZIP=../shortest-path/src/main/resources/collision-map.zip
export SPM_BANK_FILE=../shortest-path/src/main/resources/destinations/game_features/bank.tsv
export DOOR_TRANSPORTS_TSV=../shortest-path-tooling/door_transports.tsv
```

## Experiment 1: Door Chart, Ditch Removed

This keeps the door-separated collision graph, appends the door TSV as transports, and removes `Cross Wilderness Ditch` transports from the partition/reachability inputs.

```bash
export DROP_WILDERNESS_DITCH=1
unset COLLAPSE_SMALL_DOOR_COMPONENTS

rtk nix-shell --run 'cabal run metis-partition --'
rtk nix-shell --run 'cabal run metis-partition -- integrate'
```

Outputs to inspect:

```text
out/metis/kahip-partitions.csv
out/metis/kahip-components.csv
out/metis/kahip-region-graph.csv
out/metis/partition-census.json
automatic-partition-census.md
```

## Experiment 2: Remove Ditch + Collapse Small Door Components

This additionally merges raw components connected by `DOOR` where either side is below the threshold.

```bash
export DROP_WILDERNESS_DITCH=1
export COLLAPSE_SMALL_DOOR_COMPONENTS=1000

rtk nix-shell --run 'cabal run metis-partition --'
rtk nix-shell --run 'cabal run metis-partition -- integrate'
```

Try other collapse thresholds:

```bash
export COLLAPSE_SMALL_DOOR_COMPONENTS=500
export COLLAPSE_SMALL_DOOR_COMPONENTS=2000
```

## Trying Different KaHIP Refinement Settings

Initial partitioning is still the fixed 50k-tile recursive split. After that, refine existing KaHIP leaves:

```bash
rtk nix-shell --run 'cabal run metis-partition -- refine-kahip 30000 1000 10'
rtk nix-shell --run 'cabal run metis-partition -- refine-kahip 20000 1000 10'
rtk nix-shell --run 'cabal run metis-partition -- refine-kahip 20000 500 10'
rtk nix-shell --run 'cabal run metis-partition -- refine-kahip 20000 1000 25'
```

Arguments:

```text
refine-kahip MAX_LEAF_TILES MIN_CHILD_TILES MAX_CHEAP_SEPARATOR
```

Meaning:

- `MAX_LEAF_TILES`: leaves larger than this are forced to split if KaHIP finds a feasible split.
- `MIN_CHILD_TILES`: reject splits creating tiny children.
- `MAX_CHEAP_SEPARATOR`: leaves already below `MAX_LEAF_TILES` only split opportunistically if separator size is at most this.

Refinement writes isolated outputs and does not replace the main hierarchy assignments:

```text
out/metis/kahip-refine-max*-partitions.csv
out/metis/kahip-refine-max*-candidates.csv
kahip-refine-max*-report.md
```

## Reading Interface Size

After `integrate`, inspect:

```bash
less automatic-partition-census.md
```

The useful sections are:

```text
KaHIP region connectivity
Largest regions
Partitioned source components
```

For a sortable CSV view:

```bash
column -s, -t out/metis/kahip-components.csv | less -S
```

Important columns:

```text
transport_out
transport_in
interface_out
interface_in
distinct_out_regions
distinct_in_regions
```

A rough terminal/interface pressure proxy for a leaf is:

```text
origins + destinations + banks + global_destinations + interface_out + interface_in
```

The pathfinding clique problem appears when that value gets much above ~100.

## Notes

- Door transports are used for reachability and inter-component links; they do not merge raw walking components unless `COLLAPSE_SMALL_DOOR_COMPONENTS` is set.
- `DROP_WILDERNESS_DITCH=1` removes the long ditch transport interface from this experiment. That tests whether KaHIP behaves sanely without hundreds of ditch endpoints.
- Current KaHIP selection is still tile/separator-size driven. It does not yet directly optimize the ~100 interface target; use the census outputs to decide whether that extra objective is needed.
