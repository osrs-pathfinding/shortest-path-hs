# OSRS Pathfinding Graph Census

## Headline totals

- Walkable tiles: 7727358
- Walking components: 36594
- Singleton components: 30
- Components <= 10 tiles: 27150
- Components <= 100 tiles: 34441
- Components <= 1,000 tiles: 36160
- Expanded transport edges: 14215
- Distinct interesting tiles: 7345
- Components with no interesting tiles: 35544

## Component size distribution

| metric | tiles |
| --- | ---: |
| min | 1 |
| median | 4 |
| p90 | 45 |
| p95 | 135 |
| p99 | 1165 |


## Largest components

| component | tiles | bbox | banks | origins | destinations | global destinations | interesting tiles | density |
| ---: | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 29 | 2458003 | 1016,2104..3463,4167 p0 | 1 | 0 | 0 | 0 | 1 | 0.000000 |
| 30 | 1002036 | 2950,2104..3975,4167 p0 | 1 | 0 | 0 | 0 | 1 | 0.000001 |
| 27 | 514638 | 960,2048..4031,4223 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 1154 | 313005 | 1016,3039..1693,4167 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 657 | 273102 | 2260,2872..3423,3809 p0 | 67 | 1197 | 967 | 128 | 1447 | 0.005298 |
| 28 | 249286 | 1016,2104..1599,2744 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 3607 | 154703 | 2944,3522..3391,3966 p0 | 0 | 451 | 388 | 28 | 479 | 0.003096 |
| 2954 | 142878 | 1168,3399..1884,3965 p0 | 55 | 140 | 98 | 56 | 237 | 0.001659 |
| 676 | 104324 | 1183,2881..1859,3402 p0 | 14 | 63 | 59 | 37 | 98 | 0.000939 |
| 281 | 90060 | 3135,2748..3536,3168 p0 | 4 | 100 | 64 | 9 | 114 | 0.001266 |
| 1722 | 55985 | 3400,3166..3772,3583 p0 | 10 | 101 | 76 | 20 | 132 | 0.002358 |
| 15056 | 35933 | 3776,9856..3967,10047 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 5641 | 30000 | 2576,4608..2815,4799 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 848 | 26497 | 2704,2939..2960,3247 p0 | 0 | 76 | 30 | 12 | 87 | 0.003283 |
| 9525 | 24673 | 2880,7599..3071,7807 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 14023 | 20000 | 3753,9664..3903,9810 p0 | 0 | 0 | 0 | 1 | 1 | 0.000050 |
| 6341 | 18895 | 3840,4864..4031,4992 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 74 | 18296 | 3093,2308..3323,2543 p0 | 4 | 0 | 0 | 0 | 4 | 0.000219 |
| 9560 | 16758 | 2368,7680..2464,7935 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 7469 | 16670 | 1894,5846..2151,6077 p0 | 1 | 0 | 0 | 1 | 2 | 0.000120 |
| 334 | 16664 | 2086,2774..2343,3005 p0 | 1 | 10 | 3 | 1 | 11 | 0.000660 |
| 31784 | 16351 | 2176,3264..2303,3398 p2 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 35557 | 16342 | 2176,3264..2303,3397 p3 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 19904 | 16214 | 2176,3264..2303,3391 p1 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 16750 | 15013 | 3712,10112..3839,10303 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 9447 | 14629 | 1856,7040..2111,7111 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 35896 | 14277 | 3200,6016..3327,6143 p3 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 23381 | 13646 | 3200,6016..3327,6143 p1 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 32954 | 13514 | 3200,6016..3327,6150 p2 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 11459 | 13429 | 2775,8855..2919,9001 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 5962 | 13317 | 3458,4802..3581,4925 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 7268 | 12898 | 3392,5760..3519,5887 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 7015 | 12623 | 1601,5505..1726,5630 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 811 | 12466 | 3648,2930..3853,3066 p0 | 4 | 10 | 10 | 2 | 16 | 0.001283 |
| 27774 | 12291 | 2504,9600..2623,9758 p1 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 8348 | 12288 | 1600,6144..1791,6207 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 4636 | 11739 | 1486,3897..1705,4055 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 6569 | 11006 | 3840,4995..4031,5055 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 2871 | 11004 | 2369,3385..2505,3531 p0 | 0 | 228 | 62 | 7 | 245 | 0.022265 |
| 31312 | 10949 | 2368,10368..2495,10495 p1 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 13206 | 10886 | 1344,9504..1535,9663 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 9696 | 10074 | 2497,7720..2742,7873 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 199 | 10027 | 3144,2669..3270,2812 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 10496 | 10004 | 3648,8576..3775,8767 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 12138 | 9625 | 3200,9216..3455,9279 p0 | 0 | 9 | 9 | 0 | 9 | 0.000935 |
| 4220 | 9549 | 3655,3713..3829,3901 p0 | 2 | 30 | 11 | 3 | 34 | 0.003561 |
| 7969 | 9400 | 3475,6047..3617,6189 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 4730 | 9220 | 3352,3991..3493,4138 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 3433 | 9098 | 2516,3483..2667,3621 p0 | 0 | 0 | 0 | 0 | 0 | 0.000000 |
| 557 | 8894 | 1314,2842..1462,3000 p0 | 4 | 6 | 6 | 4 | 12 | 0.001349 |


## Transport categories

| category | logical families/actions | origins | destinations | expanded edges | bidirectional pairs | median cost | p95 cost | max cost |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| fixed origin -> fixed destination | 7 | 7 | 6 | 7 | 1 | 5 | 5 | 5 |
| fixed origin -> multiple destinations | 6496 | 5817 | 4546 | 6494 | 2164 |  |  | 77 |
| global or broad-origin -> multiple destinations | 430 | 0 | 302 | 430 | 0 |  |  | 23 |
| multiple origins -> multiple destinations / hub network | 8 | 366 | 122 | 6846 | 1944 |  |  | 21 |
| other/unclassified | 438 | 0 | 428 | 438 | 0 | 4 | 4 | 4 |


## Hub/network transports

| family | entrances | exits | expanded edges | walking components touched |
| --- | ---: | ---: | ---: | ---: |
| FAIRY_RING | 55 | 55 | 3134 | 2 |
| GNOME_GLIDER | 14 | 7 | 77 | 4 |
| HOT_AIR_BALLOON | 45 | 6 | 231 | 3 |
| MAGIC_MUSHTREE | 6 | 4 | 16 | 3 |
| MINECART | 35 | 16 | 1002 | 5 |
| QUETZAL | 14 | 14 | 196 | 2 |
| SPIRIT_TREE | 142 | 14 | 1860 | 9 |
| WILDERNESS_OBELISK | 55 | 6 | 330 | 1 |


## Global teleports

- Logical global teleport actions: 999
- Distinct destination tiles: 743
- Duplicate actions landing on an already-seen tile: 256
- Destination walking components: 120
- With item requirements: 831
- With quest requirements: 184
- With var requirements: 279
- Banking availability is not derivable from TSV alone; TSV only records item requirements.

| component | global destinations |
| ---: | ---: |
| 657 | 128 |
| 2954 | 56 |
| 676 | 37 |
| 3607 | 28 |
| 1722 | 20 |
| 848 | 12 |
| 281 | 9 |
| 2871 | 7 |
| 4014 | 6 |
| 7923 | 6 |
| 557 | 4 |
| 4534 | 4 |
| 15796 | 4 |
| 525 | 3 |
| 872 | 3 |
| 1173 | 3 |
| 2137 | 3 |
| 4220 | 3 |
| 4224 | 3 |
| 13147 | 3 |


## Component-level transport graph

- Same-component local transports: 2311
- Cross-component local transports: 7083
- Components with no incoming inter-component transport: 35606
- Components with no outgoing inter-component transport: 35607
- Largest weakly-connected component-graph region: 956
- In-degree median/p95/max: 2/12/1439
- Out-degree median/p95/max: 2/12/1469

## Largest-component neighbours

| component | distinct outgoing components | distinct incoming components |
| ---: | ---: | ---: |
| 29 | 0 | 0 |
| 30 | 0 | 0 |
| 27 | 0 | 0 |
| 1154 | 0 | 0 |
| 657 | 248 | 265 |
| 28 | 0 | 0 |
| 3607 | 15 | 17 |
| 2954 | 43 | 43 |
| 676 | 20 | 20 |
| 281 | 35 | 36 |
| 1722 | 32 | 32 |
| 15056 | 0 | 0 |
| 5641 | 0 | 0 |
| 848 | 30 | 31 |
| 9525 | 0 | 0 |
| 14023 | 0 | 0 |
| 6341 | 0 | 0 |
| 74 | 0 | 0 |
| 9560 | 0 | 0 |
| 7469 | 0 | 0 |


## High-degree components

| component | out-degree | in-degree |
| ---: | ---: | ---: |
| 657 | 1469 | 1439 |
| 3607 | 387 | 377 |
| 2954 | 351 | 317 |
| 2871 | 402 | 219 |
| 848 | 234 | 208 |
| 4534 | 171 | 140 |
| 525 | 121 | 157 |
| 1320 | 144 | 129 |
| 7923 | 100 | 150 |
| 245 | 108 | 132 |
| 281 | 79 | 78 |
| 1722 | 75 | 69 |
| 676 | 57 | 57 |
| 22646 | 48 | 48 |
| 872 | 45 | 49 |
| 2594 | 32 | 41 |
| 17018 | 30 | 32 |
| 888 | 32 | 28 |
| 557 | 29 | 29 |
| 6615 | 30 | 27 |


## Notes

- The census uses current resource files directly from `/home/matt/shortest-path/src/main/resources`.
- Hub classification is based on TSV permutation shape: rows with origin-only plus destination-only entries.
- `graph-census.json` and CSV files under `out/` preserve the measured details for later analysis.
