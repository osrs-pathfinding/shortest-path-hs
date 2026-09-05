# Pathfinding Benchmark Corpus and Reporting Plan

Build a reproducible benchmark suite for the OSRS pathfinder.

The benchmark system should answer two different kinds of questions:

1. **Is performance improving or regressing over time?**
2. **Why is one particular route behaving badly?**

Do not solve both with custom tooling.

Use a standard benchmarking/reporting system for trends and regression tracking.

Use the existing pathfinding viewer only for detailed inspection of individual benchmark routes.

---

# 1. Benchmark goals

We want to measure performance across a broad and realistic sample of the game.

The suite should answer:

* How fast is routing for early-, mid-, end-game and maxed accounts?
* What are median, p95, p99 and worst-case query times?
* Which routes expand the most states?
* Which account profiles create the hardest searches?
* Which changes improve search broadly rather than only improving one fixture?
* Did a commit cause a performance regression?
* Which specific route should we inspect when a regression occurs?
* Does exact A* still produce the correct route cost?

Target initially:

```text
~750 fixed route pairs
x 4 account profiles
= ~3000 route/profile cases
```

Make increasing this towards 1000 routes easy later.

---

# 2. Fixed account profiles

Create four checked-in profiles:

```text
early
mid
end
maxed
```

Each should explicitly define whatever the pathfinding model currently supports:

* skill levels;
* quest/unlock requirements;
* inventory items;
* bank items;
* teleport availability;
* transport availability.

Use the same route corpus for every profile.

A route being unreachable for one account and reachable for another is useful data.

---

# 3. Fixed and reproducible route corpus

Generate:

```text
benchmarks/corpus/routes-v1.json
```

and commit it.

Do not randomly regenerate benchmark routes during normal benchmark runs.

Use a fixed random seed during corpus generation.

Each route gets:

```text
stable route ID
raw start coordinate
resolved start coordinate
raw target coordinate
resolved target coordinate
category
endpoint names where available
endpoint provenance
distance/geographic tags
smoke / standard / full tier membership
```

For example:

```json
{
  "id": "quest-natural-0017",
  "rawStart": [3222, 3218, 0],
  "start": [3222, 3218, 0],
  "rawTarget": [3151, 3207, 0],
  "target": [3151, 3207, 0],
  "category": "quest-natural",
  "startName": "Lumbridge",
  "targetName": "Lost City quest step",
  "startSource": "gps-destination",
  "targetSource": "quest-helper",
  "tiers": ["smoke", "standard", "full"]
}
```

---

# 4. Natural endpoint sources

A large proportion of benchmark destinations should be places players would naturally route to.

## RuneLite GPS destination data

Use the existing GPS destination resources as one main source.

Extract named destinations such as:

* towns;
* banks;
* dungeons;
* minigames;
* training locations;
* landmarks;
* altars;
* transport hubs;
* other curated places.

Deduplicate nearby equivalent locations.

A bank represented by several adjacent tiles should normally become one logical benchmark destination.

Preserve:

```text
name
category
source file
raw coordinate
resolved benchmark coordinate
```

## Quest Helper

Use Quest Helper as a second source of natural locations.

Extract static `WorldPoint` locations from quest steps.

Use these primarily as:

```text
quest-step
```

destinations.

Only label something:

```text
quest-start
```

when that can be identified reliably.

Avoid allowing quests with hundreds of coded coordinates to dominate the dataset.

Select only a few representative locations per quest where practical.

This gives us realistic routes similar to those users may request through Quest Helper integration.

## Clue steps and clue locations

Use RuneLite clue-step/location data as a third natural-endpoint source. Clue
targets are especially valuable because they are often deliberately tucked into
awkward or hard-to-reach places: remote islands, caves, unusual planes, quest
gates, obstacle-heavy terrain, and transport-dependent areas.

Tag these endpoints as:

```text
clue-step
```

They should contribute to both natural routes and permanent regression fixtures.
Keep provenance precise (plugin/data source and clue type where available), and
deduplicate adjacent variants of the same clue location.

Include a useful subset of:

```text
clue -> clue
```

routes. This models a real player moving between clue objectives, rather than
only travelling from a town or bank to a clue location. It also naturally
exercises the awkward access patterns that make clue endpoints valuable.

---

# 5. Endpoint resolution

Run candidate endpoints through the same resolution/snap logic used by the real application where possible.

Store:

```text
raw coordinate
resolved coordinate
```

Do not automatically reject blocked-but-valid transport objects such as fairy-ring tiles if the routing model knows how to approach them.

---

# 6. Corpus composition

Aim for roughly 750 route pairs.

A suggested initial mix:

## 250 named natural routes

Pair human-recognisable GPS destinations.

Examples:

```text
town -> dungeon
bank -> quest area
landmark -> landmark
place -> minigame
island -> mainland
training area -> bank
```

## 150 quest-related routes

Use Quest Helper locations.

Examples:

```text
town -> quest step
bank -> quest step
quest step -> bank
quest step -> quest step
quest location -> another named location
```

If ordered quest steps can be extracted reliably, include some consecutive-step routes.

## 100 walking-focused routes

Choose points in the same natural walking component.

Include:

* short walking routes;
* long walking routes;
* open terrain;
* obstacle-heavy terrain;
* large mainland components;
* Wilderness.

Include a walking-only benchmark mode where useful.

## 75 transport/hub stress routes

Deliberately exercise:

* fairy rings;
* spirit trees;
* gliders;
* bank globals;
* initial globals;
* minigame teleports;
* other hub families.

These should include situations with several plausible transport alternatives.

## 50 Wilderness/special-area routes

Include:

```text
Wilderness <=20
Wilderness 20–30
Wilderness >30
entrances/exits
deep Wilderness
Wilderness dungeons
nearby non-Wilderness areas
```

These become especially useful once Wilderness teleport restrictions are modelled properly.

## 75 geographically stratified random routes

Do not uniformly sample all walkable tiles.

Instead:

1. divide the world into coarse geographic buckets;
2. sample buckets reasonably evenly;
3. choose reachable tiles within them;
4. construct routes across different distances and regions.

## 50 permanent regression fixtures

Keep known historically useful routes permanently.

Examples should include cases that exposed:

* dead-end component flooding;
* bank dominance;
* blocked transport origins;
* hub mushrooms;
* unusual one-way transports;
* cross-plane routes;
* Kourend -> Desert;
* any future regression.

---

# 7. Coverage dimensions

The corpus generator should report coverage across:

## Route length

For example:

```text
local
regional
long
cross-world
```

## Plane

Include:

```text
0 -> 0
0 -> non-zero
non-zero -> 0
non-zero -> non-zero
```

## Walking topology

Tag:

```text
same natural component
different components
transport-required
```

## Directionality

Include reverse directions for a useful subset:

```text
A -> B
B -> A
```

OSRS transports are often asymmetric.

## Account sensitivity

Mark routes which behave substantially differently across account profiles.

---

# 8. Benchmark tiers

Provide three fixed subsets.

## Smoke

Approximately:

```text
20 routes x 4 profiles
```

Use constantly.

Include every major semantic feature.

## Standard

Approximately:

```text
100–150 routes x 4 profiles
```

Use during algorithm development.

## Full

Approximately:

```text
750 routes x 4 profiles
```

Use for serious performance comparisons and major changes.

All tiers should use stable route IDs from the same corpus.

---

# 9. Correctness oracle

Separate correctness validation from performance measurement.

For every:

```text
route
x account profile
```

compute once using the exact reference implementation:

```text
reachable/unreachable
optimal cost
```

Store this in a checked-in/generated oracle associated with the corpus/model version.

During normal benchmark runs:

```text
A* reachable == expected reachable
```

and for exact A*:

```text
A* cost == expected optimal cost
```

Regenerate the oracle when:

* collision changes;
* transport definitions change;
* requirement semantics change;
* route cost semantics change.

Do not run raw Dijkstra repeatedly during normal performance benchmarks.

---

# 10. Benchmark execution

Create an in-process benchmark executable.

For example:

```text
cabal run route-bench
```

The canonical benchmark should not include:

* HTTP;
* JSON request parsing;
* viewer/server overhead.

Build static pathfinding structures once.

Run many queries against the same loaded world.

Report startup/precomputation separately.

For performance runs:

* perform warm-up;
* use repeated runs where appropriate;
* use median timing;
* avoid expensive diagnostic instrumentation which changes timings.

Support:

```text
performance mode
diagnostic mode
```

separately.

The local workflow is:

```text
validate selected corpus
-> generate exact oracle when model semantics change
-> run smoke / standard / full in-process
-> write JSONL
-> export aggregates and sentinels to Bencher Metric Format
-> inspect a selected route in the existing viewer when needed
```

Use the checked-in `benchmarks/routes.json` only as a temporary seed fixture for
runner tests. It is not the v1 selected corpus.

---

# 11. Raw benchmark result

Every run should emit a structured record.

JSONL is sufficient as the canonical detailed output.

Suggested fields:

```text
benchmark version
git commit
testbed
route ID
route category
account profile
repetition

start
target
reachable
cost

heuristic setup ms
reverse Dijkstra ms
seed-table ms
search ms
total ms

states popped
unique states reached
PQ pushes
stale entries
walking relaxations
transport relaxations
heuristic evaluations

unreachable prunes
unknown-component prunes
no-reverse-seed prunes

best-bank updates
final best-bank cost
bank-dominated heuristic evaluations
bank-global transitions suppressed
bank-bound PQ rekeys
```

Keep this full raw result file.

---

# 12. Standard performance reporting

Do NOT implement historical benchmark charts or trend analysis ourselves.

Use a standard benchmarking service/tool such as Bencher for:

* historical trends;
* commit-to-commit comparisons;
* regression detection;
* branch comparisons;
* machine/testbed separation.

The benchmark executable should export the selected metrics into the format expected by that system.

For Bencher, export Bencher Metric Format JSON from the JSONL run output. Track
aggregate profile percentiles and selected sentinels there; do not upload every
low-level counter for every route/profile case.

The standard reporting system owns:

```text
"Did performance regress?"
"How has p95 changed over time?"
"Which commit changed this benchmark?"
"Is this machine statistically slower?"
```

Do not put this functionality into the pathfinding viewer.

---

# 13. What to report historically

Do not create a historical time series for every low-level counter on every one of the ~3000 cases.

Track two useful layers.

## Aggregate corpus metrics

For each account profile track things like:

```text
total time p50
total time p95
total time p99

search time p50
search time p95
search time p99

reverse setup p50
reverse setup p95
reverse setup p99

expansions p50
expansions p95
expansions p99
```

Also track a few useful categories if they prove meaningful:

```text
walking
quest-natural
transport-heavy
Wilderness
```

## Sentinel routes

Choose around 30–50 stable route/profile cases.

Track these individually over time.

Sentinels should cover:

* important regressions;
* known pathological routes;
* major hub families;
* walking-heavy cases;
* bank-global-heavy cases;
* Wilderness;
* cross-plane routes;
* Kourend -> Desert.

For each sentinel track at least:

```text
total time
search time
reverse setup time
states popped
```

---

# 14. The existing viewer is for individual route diagnosis

Integrate the benchmark corpus into the current pathfinding viewer.

This is the only bespoke benchmark UI we should build.

The viewer's purpose is:

> explain why a particular benchmark route behaves the way it does.

It is NOT a generic performance dashboard.

## Benchmark route picker

Allow the viewer to load a benchmark case by:

```text
route ID
account profile
```

For example:

```text
?benchmark=quest-natural-0017&profile=mid
```

The route selector should allow simple filtering by:

```text
category
account profile
route ID/name
```

This is primarily a fixture browser.

The local viewer should also list the latest JSONL benchmark result fixtures.
Selecting one restores its endpoints and recorded account profile before the
user runs it again with detailed tracing enabled.

## Route inspection

Once loaded, use the existing pathfinding visualisations to inspect:

* start and target;
* chosen path;
* expanded tiles;
* expansion order;
* heuristic layers;
* heuristic contours;
* heuristic seeds;
* transport locations;
* bank lifecycle;
* dominance events;
* unreachable pruning;
* hub-related behaviour;
* any debug data already supported by the viewer.

This is useful bespoke tooling because generic benchmark products cannot explain OSRS route geometry.

---

# 15. Link standard benchmark reporting to the viewer

Every individually tracked benchmark/sentinel should have a stable route ID.

When a historical regression system says:

```text
bank-global-0027 / maxed
search time: 80 ms -> 190 ms
states popped: 9k -> 31k
```

it should be trivial to open that exact case in the pathfinding viewer.

Where supported, expose a link such as:

```text
/viewer?benchmark=bank-global-0027&profile=maxed
```

The workflow should be:

```text
standard benchmark reporting
        |
        | identifies regression
        v
specific route ID
        |
        v
pathfinding viewer
        |
        v
inspect search behaviour
```

---

# 16. Do not build these things ourselves

Do NOT implement custom viewer pages for:

* performance-over-time charts;
* commit comparisons;
* p50/p95/p99 trend graphs;
* regression thresholding;
* machine comparisons;
* benchmark history;
* statistical significance.

Use the standard reporting tool.

Do NOT turn the viewer into a general benchmark dashboard.

---

# 17. Ad-hoc analysis

Detailed one-off analysis can simply operate on the raw JSONL output with whatever standard tool is convenient.

Do not make a permanent data-analysis platform part of the project unless a concrete need emerges.

The benchmark architecture should not depend on DuckDB, notebooks, or a custom analytics database.

Those can be used opportunistically later.

---

# 18. Testbed identity

Benchmark results must identify the machine/testbed.

Do not compare:

```text
desktop
laptop
CI VM
```

as though they were the same performance series.

Use one stable machine for serious performance comparisons where possible.

---

# 19. Corpus coverage report

The corpus generator should produce a text summary before a new corpus is accepted.

Report:

```text
total routes

routes by category
routes by endpoint source
routes by distance bucket
routes by plane relationship
same-component routes
cross-component routes
quest-derived routes
Wilderness routes
transport/hub routes
bidirectional pairs
unique endpoints
```

Also report coarse geographic coverage.

This is how we avoid accidentally constructing a benchmark which mostly measures ordinary mainland routes.

---

# 20. Versioning and provenance

Pin external endpoint sources.

For Quest Helper store the upstream commit used during extraction.

For GPS destination data store the source revision.

Do not silently regenerate `routes-v1.json` when upstream changes.

Create:

```text
routes-v2.json
```

for intentional corpus refreshes.

---

# 21. Deliverables

Implement:

1. four checked-in account profiles;
2. deterministic GPS destination extractor;
3. deterministic Quest Helper location extractor;
4. endpoint normalisation/deduplication;
5. route-corpus generator;
6. approximately 750-route `routes-v1.json`;
7. smoke / standard / full subsets;
8. exact correctness oracle;
9. in-process route benchmark executable;
10. detailed JSONL benchmark output;
11. exporter/integration for a standard historical benchmark system;
12. aggregate historical metrics;
13. 30–50 sentinel benchmarks;
14. benchmark route/profile loading in the existing viewer;
15. stable links/IDs for opening regressions in the viewer;
16. corpus coverage report.

Until route selection is complete, keep `routes-v1.json` and the sentinel list
as checked-in empty placeholders. The infrastructure, schema validation,
in-process runner, oracle generation, JSONL/BMF export, and viewer fixture
loading can be developed and tested against the existing seed routes first.

## Design principle

Keep the responsibilities separate:

```text
standard benchmarking framework:
    history
    trends
    regression detection
    comparisons

pathfinding viewer:
    inspect one route
    understand geometry
    understand heuristic/search behaviour
```

Do not reinvent generic benchmark-analysis tooling.

Only build custom UI where the problem is genuinely OSRS/pathfinding-specific.
