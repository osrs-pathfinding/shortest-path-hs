# METIS and KaHIP Walking-Graph Partitioning

METIS 5.2.1 (`gpmetis`) and KaHIP 3.23 (`node_separator --preconfiguration=strong --imbalance=20 --seed=42`) use METIS-format graphs containing only legal raw walking edges; transports and virtual walls are excluded. Recursive leaves stop at 50,000 tiles.

The current manual wall split is intentionally not applied to these input graphs; it remains a separate comparison baseline because it changes topology rather than partitioning the original graph.

## Component 281

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 90060 | 44588 | 45472 | 11 | 5 | 5 | 56 | 59 |

METIS leaves: 2

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 45446 | 44609 | 5 | 59 | 56 | 0 | 0.000112 |


## Component 657

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 273102 | 136946 | 136156 | 1 | 1 | 1 | 520 | 932 |
| 1a | 136946 | 68473 | 68473 | 28 | 16 | 16 | 233 | 287 |
| 1aa | 68473 | 34545 | 33928 | 22 | 14 | 14 | 145 | 88 |
| 1ab | 68473 | 34181 | 34292 | 25 | 13 | 13 | 232 | 55 |
| 1b | 136156 | 68570 | 67586 | 42 | 22 | 22 | 355 | 577 |
| 1ba | 68570 | 34245 | 34325 | 35 | 21 | 21 | 139 | 216 |
| 1bb | 67586 | 33288 | 34298 | 34 | 20 | 22 | 273 | 304 |

METIS leaves: 8

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 136106 | 136995 | 1 | 931 | 521 | 0 | 0.000007 |
| 1a | 79309 | 56789 | 8 | 523 | 408 | 0 | 0.000141 |
| 1aa | 37573 | 41722 | 14 | 256 | 267 | 0 | 0.000373 |
| 1ab | 27492 | 29284 | 13 | 319 | 89 | 0 | 0.000473 |
| 1b | 73297 | 63690 | 8 | 272 | 249 | 0 | 0.000126 |
| 1ba | 43735 | 29555 | 7 | 197 | 75 | 0 | 0.000237 |
| 1bb | 36014 | 27672 | 4 | 221 | 28 | 0 | 0.000145 |


## Component 676

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 104324 | 52285 | 52039 | 17 | 9 | 9 | 24 | 74 |
| 1a | 52285 | 26261 | 26024 | 161 | 79 | 77 | 5 | 19 |
| 1b | 52039 | 26048 | 25991 | 119 | 69 | 61 | 28 | 46 |

METIS leaves: 4

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 62090 | 42232 | 2 | 38 | 60 | 0 | 0.000047 |
| 1a | 29764 | 32295 | 31 | 24 | 14 | 0 | 0.001042 |


## Component 1722

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 55985 | 28607 | 27378 | 10 | 6 | 6 | 76 | 56 |

METIS leaves: 2

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 29197 | 26785 | 3 | 76 | 56 | 0 | 0.000112 |


## Component 2954

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 142878 | 71423 | 71455 | 54 | 30 | 30 | 135 | 102 |
| 1a | 71423 | 35553 | 35870 | 19 | 11 | 11 | 82 | 53 |
| 1b | 71455 | 36394 | 35061 | 28 | 12 | 12 | 69 | 33 |

METIS leaves: 4

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 81780 | 61083 | 15 | 166 | 71 | 0 | 0.000246 |
| 1a | 48405 | 33366 | 9 | 84 | 82 | 0 | 0.000270 |
| 1b | 30660 | 30423 | 0 | 45 | 26 | 0 | 0.000000 |


## Component 3607

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 154703 | 77351 | 77352 | 419 | 207 | 202 | 254 | 225 |
| 1a | 77351 | 39727 | 37624 | 228 | 120 | 110 | 20 | 234 |
| 1b | 77352 | 38733 | 38619 | 157 | 98 | 93 | 58 | 167 |

METIS leaves: 4

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 78973 | 75591 | 139 | 213 | 266 | 0 | 0.001839 |
| 1a | 37370 | 41535 | 68 | 161 | 52 | 0 | 0.001820 |
| 1b | 37274 | 38271 | 46 | 245 | 21 | 0 | 0.001234 |


