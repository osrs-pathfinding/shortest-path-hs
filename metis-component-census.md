# METIS Region Census

Walking components are the METIS recursive leaf regions. Every crossed walking edge is represented in `out/metis/region-transports.csv` by two `VIRTUAL_CUT` transports at cost 1 tick; no tile is removed.

| original component | original tiles | regions | largest region | smallest region |
|---|---:|---:|---:|---:|
| 281 | 90060 | 2 | 45472 | 44588 |
| 657 | 273102 | 8 | 34545 | 33288 |
| 676 | 104324 | 4 | 26261 | 25991 |
| 1722 | 55985 | 2 | 28607 | 27378 |
| 2954 | 142878 | 4 | 36394 | 35061 |
| 3607 | 154703 | 4 | 39727 | 37624 |

Detailed region records: `out/metis/region-nodes.csv`

## Region details

| component | region | tiles | interesting |
|---|---|---:|---:|
| 281 | 1a | 44588 | 56 |
| 281 | 1b | 45472 | 59 |
| 657 | 1aaa | 34545 | 145 |
| 657 | 1aab | 33928 | 88 |
| 657 | 1aba | 34181 | 232 |
| 657 | 1abb | 34292 | 55 |
| 657 | 1baa | 34245 | 139 |
| 657 | 1bab | 34325 | 216 |
| 657 | 1bba | 33288 | 273 |
| 657 | 1bbb | 34298 | 304 |
| 676 | 1aa | 26261 | 5 |
| 676 | 1ab | 26024 | 19 |
| 676 | 1ba | 26048 | 28 |
| 676 | 1bb | 25991 | 46 |
| 1722 | 1a | 28607 | 76 |
| 1722 | 1b | 27378 | 56 |
| 2954 | 1aa | 35553 | 82 |
| 2954 | 1ab | 35870 | 53 |
| 2954 | 1ba | 36394 | 69 |
| 2954 | 1bb | 35061 | 33 |
| 3607 | 1aa | 39727 | 20 |
| 3607 | 1ab | 37624 | 234 |
| 3607 | 1ba | 38733 | 58 |
| 3607 | 1bb | 38619 | 167 |
