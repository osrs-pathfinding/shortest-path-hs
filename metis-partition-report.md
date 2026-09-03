# METIS and KaHIP Walking-Graph Partitioning

METIS (`gpmetis`) and KaHIP (`node_separator --preconfiguration=strong --imbalance=20 --seed=42`) use raw walking-only graphs. Recursive leaves stop at 50,000 tiles.

## Component 304

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|

METIS leaves: 0

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 42565 | 45420 | 5 | 48 | 55 | 0 | 0.000117 |

## Component 678

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|

METIS leaves: 0

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 77337 | 73717 | 7 | 295 | 819 | 0 | 0.000095 |
| 1a | 37489 | 39841 | 7 | 200 | 95 | 0 | 0.000187 |
| 1b | 36828 | 36885 | 4 | 508 | 311 | 0 | 0.000109 |

## Component 697

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|

METIS leaves: 0

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 61385 | 42937 | 2 | 98 | 130 | 0 | 0.000047 |
| 1a | 31564 | 29790 | 31 | 33 | 65 | 0 | 0.001041 |

## Component 1480

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|

METIS leaves: 0

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 57777 | 50955 | 8 | 302 | 586 | 0 | 0.000157 |
| 1a | 24557 | 33206 | 14 | 152 | 150 | 0 | 0.000570 |
| 1b | 25469 | 25477 | 9 | 196 | 390 | 0 | 0.000353 |

## Component 1825

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|

METIS leaves: 0

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 31211 | 24773 | 1 | 92 | 78 | 0 | 0.000040 |

## Component 3405

### METIS

| split | parent | child A | child B | cut edges | boundary A | boundary B | interesting A | interesting B |
|---|---:|---:|---:|---:|---:|---:|---:|---:|

METIS leaves: 0

### KaHIP node separators

| split | side A | side B | separator | interesting A | interesting B | interesting S | separator/min side |
|---|---:|---:|---:|---:|---:|---:|---:|
| 1 | 80169 | 61046 | 15 | 209 | 305 | 0 | 0.000246 |
| 1a | 36187 | 43970 | 12 | 119 | 90 | 0 | 0.000332 |
| 1b | 30523 | 30521 | 2 | 72 | 233 | 0 | 0.000066 |

