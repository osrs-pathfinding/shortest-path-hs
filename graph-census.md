# OSRS Pathfinding Graph Census

## Headline totals

- Scope: components reachable from Lumbridge (`3221 3218 0`) via directed transports, including broad/global teleports.
- Raw walkable tiles before reachability filter: 7727352
- Raw walking components before reachability filter: 36595
- Excluded disconnected components: 35629
- Walkable tiles: 1346253
- Walking components: 966
- Singleton components: 0
- Components <= 10 tiles: 106
- Components <= 100 tiles: 562
- Components <= 1,000 tiles: 841
- Expanded transport edges: 14221
- Distinct interesting tiles: 7351
- Components with no interesting tiles: 0

## Component size distribution

| metric | tiles |
| --- | ---: |
| min | 2 |
| median | 56 |
| p90 | 1408 |
| p95 | 2608 |
| p99 | 16664 |


## Largest components

| component | tiles | bbox | banks | origins | destinations | global destinations | interesting tiles | density |
| ---: | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 3608 | 154703 | 2944,3522..3391,3966 p0 | 0 | 451 | 388 | 28 | 479 | 0.003096 |
| 2955 | 142878 | 1168,3399..1884,3965 p0 | 55 | 140 | 98 | 56 | 237 | 0.001659 |
| 657 | 136945 | 2260,2872..2866,3809 p0 | 26 | 418 | 266 | 59 | 520 | 0.003797 |
| 1407 | 136151 | 2790,3106..3423,3606 p0 | 41 | 784 | 706 | 69 | 932 | 0.006845 |
| 676 | 104324 | 1183,2881..1859,3402 p0 | 14 | 63 | 59 | 37 | 98 | 0.000939 |
| 281 | 90060 | 3135,2748..3536,3168 p0 | 4 | 101 | 65 | 9 | 115 | 0.001277 |
| 1723 | 55985 | 3400,3166..3772,3583 p0 | 10 | 101 | 76 | 20 | 132 | 0.002358 |
| 848 | 26497 | 2704,2939..2960,3247 p0 | 0 | 76 | 30 | 12 | 87 | 0.003283 |
| 14024 | 20000 | 3753,9664..3903,9810 p0 | 0 | 0 | 0 | 1 | 1 | 0.000050 |
| 7470 | 16670 | 1894,5846..2151,6077 p0 | 1 | 0 | 0 | 1 | 2 | 0.000120 |
| 334 | 16664 | 2086,2774..2343,3005 p0 | 1 | 10 | 3 | 1 | 11 | 0.000660 |
| 811 | 12466 | 3648,2930..3853,3066 p0 | 4 | 10 | 10 | 2 | 16 | 0.001283 |
| 2872 | 11004 | 2369,3385..2505,3531 p0 | 0 | 228 | 62 | 7 | 245 | 0.022265 |
| 4221 | 9549 | 3655,3713..3829,3901 p0 | 2 | 30 | 11 | 3 | 34 | 0.003561 |
| 557 | 8894 | 1314,2842..1462,3000 p0 | 4 | 6 | 6 | 4 | 12 | 0.001349 |
| 5788 | 8649 | 3274,4751..3379,4853 p0 | 0 | 6 | 5 | 0 | 6 | 0.000694 |
| 678 | 8424 | 2757,2881..2974,2946 p0 | 0 | 18 | 14 | 1 | 18 | 0.002137 |
| 16081 | 7347 | 3145,10054..3261,10234 p0 | 0 | 9 | 10 | 2 | 14 | 0.001906 |
| 8702 | 7227 | 2564,6306..2719,6459 p0 | 0 | 0 | 0 | 1 | 1 | 0.000138 |
| 7924 | 6711 | 3207,6024..3320,6138 p0 | 8 | 24 | 19 | 6 | 37 | 0.005513 |
| 888 | 6188 | 2889,2950..3004,3119 p0 | 0 | 6 | 3 | 2 | 7 | 0.001131 |
| 259 | 5857 | 2692,2728..2811,2811 p0 | 1 | 7 | 6 | 1 | 9 | 0.001537 |
| 4539 | 5833 | 2057,3844..2167,3956 p0 | 3 | 1 | 2 | 2 | 6 | 0.001029 |
| 15797 | 5485 | 1601,9985..1729,10104 p0 | 0 | 30 | 12 | 4 | 30 | 0.005469 |
| 2283 | 5116 | 2433,3266..2556,3334 p0 | 0 | 27 | 22 | 1 | 28 | 0.005473 |
| 16037 | 5086 | 3330,10050..3451,10171 p0 | 0 | 6 | 2 | 0 | 6 | 0.001180 |
| 17019 | 5029 | 2819,10157..2941,10237 p0 | 2 | 34 | 23 | 1 | 39 | 0.007755 |
| 4535 | 4934 | 2498,3837..2625,3902 p0 | 0 | 41 | 10 | 4 | 45 | 0.009120 |
| 12477 | 4850 | 3716,9349..3837,9469 p0 | 0 | 18 | 3 | 1 | 18 | 0.003711 |
| 28168 | 4570 | 3724,9672..3898,9836 p1 | 0 | 18 | 17 | 0 | 18 | 0.003939 |
| 5137 | 4464 | 2372,4368..2493,4478 p0 | 5 | 4 | 5 | 1 | 11 | 0.002464 |
| 4408 | 4457 | 2309,3779..2425,3900 p0 | 1 | 23 | 11 | 1 | 26 | 0.005834 |
| 2395 | 4299 | 1091,3287..1236,3453 p0 | 0 | 2 | 1 | 1 | 3 | 0.000698 |
| 14072 | 3860 | 2880,9671..2969,9853 p0 | 0 | 15 | 15 | 2 | 16 | 0.004145 |
| 5777 | 3854 | 2957,4745..3124,4849 p0 | 0 | 1 | 1 | 0 | 2 | 0.000519 |
| 14210 | 3568 | 2962,9699..3061,9852 p0 | 1 | 13 | 13 | 2 | 16 | 0.004484 |
| 12463 | 3363 | 3177,9346..3325,9404 p0 | 0 | 8 | 5 | 0 | 8 | 0.002379 |
| 6614 | 3345 | 2625,5057..2749,5118 p0 | 0 | 4 | 1 | 0 | 5 | 0.001495 |
| 525 | 3303 | 2454,2832..2604,2907 p0 | 1 | 19 | 11 | 3 | 20 | 0.006055 |
| 12739 | 3263 | 2694,9412..2750,9510 p0 | 0 | 2 | 2 | 1 | 2 | 0.000613 |
| 14514 | 3256 | 2817,9761..2927,9854 p0 | 0 | 11 | 9 | 2 | 12 | 0.003686 |
| 208 | 3209 | 2694,2690..2812,2765 p0 | 0 | 2 | 1 | 0 | 2 | 0.000623 |
| 4234 | 3174 | 2817,3718..2902,3835 p0 | 0 | 3 | 1 | 0 | 3 | 0.000945 |
| 11777 | 3156 | 2690,9090..2813,9149 p0 | 0 | 0 | 1 | 1 | 1 | 0.000317 |
| 17596 | 2996 | 2400,10371..2489,10462 p0 | 0 | 2 | 2 | 0 | 2 | 0.000668 |
| 32516 | 2888 | 2826,5279..2912,5364 p2 | 0 | 4 | 2 | 1 | 5 | 0.001731 |
| 6690 | 2854 | 1732,5133..1787,5244 p0 | 0 | 0 | 0 | 1 | 1 | 0.000350 |
| 872 | 2844 | 2817,2945..2878,3006 p0 | 2 | 37 | 27 | 3 | 46 | 0.016174 |
| 22647 | 2651 | 2691,5251..2748,5372 p1 | 0 | 44 | 26 | 0 | 44 | 0.016598 |
| 15801 | 2608 | 2690,9989..2810,10042 p0 | 0 | 8 | 6 | 2 | 9 | 0.003451 |


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
| GNOME_GLIDER | 14 | 7 | 77 | 5 |
| HOT_AIR_BALLOON | 45 | 6 | 231 | 4 |
| MAGIC_MUSHTREE | 6 | 4 | 16 | 3 |
| MINECART | 35 | 16 | 1002 | 5 |
| QUETZAL | 14 | 14 | 196 | 2 |
| SPIRIT_TREE | 142 | 14 | 1860 | 10 |
| WILDERNESS_OBELISK | 55 | 6 | 330 | 1 |


## Global teleports

- Logical global teleport actions: 999
- Distinct destination tiles: 743
- Duplicate actions landing on an already-seen tile: 256
- Destination walking components: 121
- With item requirements: 831
- With quest requirements: 184
- With var requirements: 279
- Banking availability is not derivable from TSV alone; TSV only records item requirements.

| component | global destinations |
| ---: | ---: |
| 1407 | 69 |
| 657 | 59 |
| 2955 | 56 |
| 676 | 37 |
| 3608 | 28 |
| 1723 | 20 |
| 848 | 12 |
| 281 | 9 |
| 2872 | 7 |
| 4015 | 6 |
| 7924 | 6 |
| 557 | 4 |
| 4535 | 4 |
| 15797 | 4 |
| 525 | 3 |
| 872 | 3 |
| 1173 | 3 |
| 2138 | 3 |
| 4221 | 3 |
| 4225 | 3 |


## Component-level transport graph

- Same-component local transports: 2131
- Cross-component local transports: 7105
- Components with no incoming inter-component transport: 19
- Components with no outgoing inter-component transport: 28
- Largest weakly-connected component-graph region: 944
- In-degree median/p95/max: 2/13/1005
- Out-degree median/p95/max: 2/12/979

## Largest-component neighbours

| component | distinct outgoing components | distinct incoming components |
| ---: | ---: | ---: |
| 3608 | 15 | 17 |
| 2955 | 44 | 44 |
| 657 | 109 | 114 |
| 1407 | 160 | 170 |
| 676 | 21 | 21 |
| 281 | 35 | 35 |
| 1723 | 32 | 32 |
| 848 | 31 | 32 |
| 14024 | 0 | 0 |
| 7470 | 0 | 0 |
| 334 | 4 | 4 |
| 811 | 18 | 18 |
| 2872 | 38 | 40 |
| 4221 | 6 | 6 |
| 557 | 12 | 12 |
| 5788 | 2 | 2 |
| 678 | 3 | 3 |
| 16081 | 1 | 1 |
| 8702 | 0 | 0 |
| 7924 | 12 | 12 |


## High-degree components

| component | out-degree | in-degree |
| ---: | ---: | ---: |
| 1407 | 979 | 1005 |
| 657 | 643 | 574 |
| 3608 | 387 | 377 |
| 2955 | 351 | 317 |
| 2872 | 402 | 219 |
| 848 | 234 | 208 |
| 4535 | 171 | 140 |
| 525 | 121 | 157 |
| 1320 | 144 | 129 |
| 7924 | 100 | 150 |
| 245 | 108 | 132 |
| 281 | 80 | 78 |
| 1723 | 75 | 69 |
| 676 | 57 | 57 |
| 22647 | 48 | 48 |
| 872 | 45 | 49 |
| 2595 | 32 | 41 |
| 17019 | 30 | 32 |
| 888 | 32 | 28 |
| 557 | 29 | 29 |


## Notes

- The census uses current resource files directly from `/home/matt/shortest-path/src/main/resources`.
- Hub classification is based on TSV permutation shape: rows with origin-only plus destination-only entries.
- `graph-census.json` and CSV files under `out/` preserve the measured details for later analysis.
