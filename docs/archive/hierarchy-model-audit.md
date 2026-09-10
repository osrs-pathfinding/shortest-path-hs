# Hierarchy Model Audit (Historical)

This records a removed prototype and does not describe the maintained routing architecture.

Scope: H0 of `main-algorithm-plan.md`. This is an audit of the current
Haskell prototype and the Java production model; it changes no runtime
semantics.

## Executive summary

The current Haskell solver is an exact Dijkstra over its own simplified model,
but that model is not yet an account-state oracle. It parses item, quest,
varbit, varplayer, skill, wilderness-limit, and consumable fields, then ignores
all of them during search. `banked` is a structural search bit only. Global
teleports are represented as origin-less edges available from every expanded
tile. Hub/network rows are already expanded into ordinary origin-to-destination
edges, with no logical family or reuse state.

Milestone 1 should therefore prove hierarchy preservation against a raw Haskell
oracle with the same simplified semantics. It must keep expanded hub edges,
defer `usedHubMask`, and defer item/quest/var/skill/wilderness eligibility until
the flat oracle and query state are upgraded together.

## Requirement status

Status meanings: `implemented` means representable and enforced by the current
Haskell model; `missing` means the data or operation is absent; `requires
oracle change` means hierarchy work must wait for matching flat-oracle/query
semantics or it cannot be tested as exact.

| `main-algorithm-plan.md` requirement | Status | Current evidence | Consequence |
| --- | --- | --- | --- |
| Raw walking graph with legal 8-way steps | implemented | `World.hs:88-116`; raw/wall modes are explicit | H1/H2 can use `walkingNeighborsRaw`; exclude virtual walls. |
| Reachable-from-Lumbridge scope | implemented by census, not query | `GraphCensus.hs` reachable filtering; `World.hs:88-116` supplies walking | Partition loader must rebuild/validate this scope without wall topology. |
| KaHIP leaf/separator ownership | missing in core query | `MetisPartition.hs` and generated CSVs contain assignments; no hierarchy module is present | H1 must load assignments and validate one owner per raw reachable tile. |
| Leaf terminals and spatial deduplication | missing | No terminal type or discovery pass exists; `World.hs:18-26` only stores banks/transports/globals | H2 must derive terminals from current world data and separator adjacency. |
| Exact leaf metric closure | missing | `Exact/Dijkstra.hs:23-76` searches whole wall-aware world; no leaf-restricted BFS | H2 must add leaf-local mutable BFS and retain distances only. |
| Explicit separator/gateway graph | missing | `World.hs:88-116` exposes raw neighbours but no separator graph | H2/H4 must preserve every raw leaf-separator adjacency. |
| Local transport edges with query filtering and penalties | partially implemented | `Transport.hs:29-45`; `Exact/Dijkstra.hs:55-68` filters type and applies penalties | H4 can preserve this exact subset; it must exclude `VIRTUAL_WALL` from the raw oracle. |
| Expanded hub/network edges | implemented | `Transport.hs:99-119` creates permutations; `Transport.hs:142-155` merges rows | Keep expanded in milestone 1; do not infer a hub abstraction yet. |
| Logical hub family identity | missing | `Transport` has type/display/source but no family/action ID (`Transport.hs:29-45`) | Later data contract needs family ID, action identity, endpoints, and cost rule. |
| Hub cost decomposition | not established | permutation cost is merged with `max` duration (`Transport.hs:142-151`); no family cost matrix is retained | Compare expanded costs first; only compress a family after measuring its matrix. |
| Hub reuse/one-use semantics | requires oracle change | `Exact/Dijkstra.hs:15-16` state is only `(Tile, Bool)`; no use mask | Do not add `usedHubMask` in hierarchy until raw oracle has the same rule. |
| Banking transition | partially implemented | `Exact/Dijkstra.hs:49-54` flips `False -> True` at a bank at zero cost | Keep the bit for parity, but document that it unlocks nothing today. |
| Bank-obtainable item transitions | missing; requires oracle change | Haskell `Query` has no inventory/items (`Pathfinder.hs:14-21`); Dijkstra never evaluates `Transport` requirements (`Exact/Dijkstra.hs:55-68`) | Query state needs account inventory/capability and a bank transition policy before exact claims. |
| Item requirement evaluation | missing; requires oracle change | Parsing only: `Requirements.hs:17-21,46-62`; Java evaluates alternatives/quantities at `PathfinderConfig.java:968-1095` | Parsed fields cannot be treated as enabled/disabled without a shared evaluator. |
| Quest requirement evaluation | missing; requires oracle change | Haskell parses names only (`Requirements.hs:43-44`); Java checks quest state at `PathfinderConfig.java:627-635,723-731` | Add explicit query quest state to both oracle and hierarchy later. |
| Varbit/varplayer evaluation | missing; requires oracle change | Haskell parses operators (`Requirements.hs:26-32,64-74`) but never checks them; Java checks values at `PathfinderConfig.java:639-661,733-741` | Add immutable query var maps and exact operator semantics later. |
| Skill requirement evaluation | missing; requires oracle change | Haskell only parses (`Requirements.hs:23-24,35-41`); Java checks boosted levels at `PathfinderConfig.java:897-917` | Add query skill levels, including total/combat/quest-point fields if parity is required. |
| Global/broad teleport destinations | partially implemented under simplified semantics | `World.hs:172-177` splits `origin == Nothing`; `Exact/Dijkstra.hs:55-64` considers all globals from every tile | H4 may model globals only at start/explicit arrivals as the spec permits for the current universal model. |
| Global teleport eligibility by location | missing; requires oracle change | Haskell has no predicate; Java filters destinations and applies movement restrictions (`PathfinderConfig.java:442-464,534-540`; `WildernessChecker.java:30-67`) | Query/oracle need `canUseGlobalTeleport(tile,state)` or an equivalent explicit predicate and boundary terminals. |
| Wilderness-state semantics | missing; requires oracle change | Haskell has no wilderness fields/checks; Java tracks level and target exception (`Pathfinder.java:49-70,250-270`; `PathfinderConfig.java:534-540`; `Transport.java:347-352`) | Do not claim Java wilderness parity in milestone 1. |
| Transport availability by account/query state | missing in Haskell | Java builds separate without-bank/with-bank indexes (`PathfinderConfig.java:484-531`), then uses them during search (`Pathfinder.java:168-210`) | H4 must use the Haskell subset only; later availability must be shared by flat and hierarchical solvers. |
| Exact source/target attachment | missing | Current Dijkstra starts/ends on concrete tiles only (`Exact/Dijkstra.hs:23-37`) | H4 needs temporary source/target attachments and a same-leaf direct candidate. |
| Exact route reconstruction after compression | missing | Current predecessor chain stores concrete `RouteStep`s (`Exact/Dijkstra.hs:18,70-76`) | H4 must rerun leaf-local BFS and validate reconstructed cost. |
| Banking-state dominance | missing | Current search tracks separate `(Tile, Bool)` states but has no dominance rule (`Exact/Dijkstra.hs:15,28-43`) | Add only after hierarchy state is correct; dominance must include all future capability state. |
| Instrumentation | missing | `Route` has only expanded node count (`Pathfinder.hs:27-31`) | Add solver-specific stats after correctness, without changing queue order. |

## Semantic findings

### Global teleports and wilderness

Haskell stores all origin-less transports in `worldGlobalTeleports`
(`World.hs:172-177`). Dijkstra appends that entire list to neighbours for every
state whenever transports are enabled (`Exact/Dijkstra.hs:45-64`). There is no
location, wilderness, target, sailing, or account-state predicate.

Java has several distinct rules. Destination filtering removes wilderness
destinations when `avoidWilderness` is active (`PathfinderConfig.java:442-464`).
Walking into wilderness is blocked only for a non-wilderness target and only on
the boundary crossing (`PathfinderConfig.java:534-540`). The running wilderness
allowance is reduced as the search leaves level-30, level-20, and wilderness
areas (`Pathfinder.java:250-270`), and teleport usability compares that level
with each transport's maximum (`Transport.java:347-352`). This is stateful
location semantics, not a property of the destination tile alone.

The hierarchy must not hide this distinction behind a global hub. Later query
state needs at least current tile/eligibility, target wilderness status, the
teleport policy, and the transport's wilderness limit. Eligibility changes
inside a leaf require allowed-side boundary terminals, or a partition refinement.

### Bank-obtainable items

Haskell's only state transition is a free bank toggle when the current tile is
in `worldBanks` (`Exact/Dijkstra.hs:49-54`). Transport edges preserve that bit
and never inspect it (`Exact/Dijkstra.hs:59-68`). Thus the current `bankPathEnabled`
field controls whether banking is possible, not which transport becomes
available.

Java constructs separate transport sets before and after banking
(`PathfinderConfig.java:484-531`). Item checking can inspect inventory,
equipment, rune pouch, and bank (`PathfinderConfig.java:968-1045`), and then
evaluates item alternatives and quantities (`PathfinderConfig.java:1046-1095`).
The separate `BankPickupRequirements` code also treats multiple transports for
one edge as alternatives and reports the cheapest bank-satisfiable pickup
(`BankPickupRequirements.java:24-30,91-184`). This is more than a Boolean bank
flag: it is an inventory/capability transition with item substitutions and
consumable policy.

### Requirement evaluation

The Haskell parser preserves only a lightweight textual model. `parseItems`
uppercases and parses `name=quantity` expressions (`Requirements.hs:46-62`),
while skills, quests, and vars are parsed into values (`Requirements.hs:35-44,
64-74`). No evaluator is exported or called by Dijkstra. The Java model applies
transport-type rules, POH rules, teleport-item settings, jewellery-box tiers,
skills, quests, vars, and special planted-tree checks in `useTransport`
(`PathfinderConfig.java:663-748`), with item evaluation below it.

Therefore requirements are a hard oracle boundary. A hierarchy can carry
requirement metadata now, but cannot use it and still be an exact Java-semantic
implementation until the flat Haskell oracle has the same query state and
predicates.

### Logical hubs and reuse

The loader turns origin-only and destination-only rows into pairwise transports
when their endpoints pass the type radius (`Transport.hs:99-119`). The merged
transport keeps one ordinary duration and combines requirements
(`Transport.hs:142-155`). This is a graph expansion, not a logical-family
model. `TransportType` is a broad category (`Transport.hs:21-27`), not an
individual hub identity.

The Java loader has the same fundamental expansion, explicitly describing
origin/destination permutations and their radius threshold
(`TransportLoader.java:58-115`). `TransportAvailability` groups the resulting
objects by origin and keeps origin-less entries as `usableTeleports`
(`TransportAvailability.java:16-27,77-127`). The current search state contains
bank status but no used-family mask (`Pathfinder.java:168-210`; Haskell
`Exact/Dijkstra.hs:15-16`). Reusing a fairy-ring, spirit-tree, or other network
is consequently not represented as a one-use restriction in either current
search model.

Milestone 1 must keep these expanded edges. A later hub abstraction needs a
stable family/action ID, endpoint sets, directedness, base cost matrix, query
penalty rule, and explicit reuse/consumption semantics. It must prove that a
hub-node decomposition preserves every expanded edge cost before replacing
anything.

### Manual virtual walls versus raw oracle

`walkingNeighbors` applies the hardcoded virtual-wall blocked edges, while
`walkingNeighborsRaw` does not (`World.hs:88-116`). `loadWorld` also injects
bidirectional `VIRTUAL_WALL` transports (`World.hs:121-159,198-218`). Existing
Dijkstra calls the wall-aware function (`Exact/Dijkstra.hs:48`), so it is not
the raw walking oracle required by the hierarchy specification. The partition
experiment and KaHIP graph use raw walking adjacency; the hierarchy baseline
must therefore add a separate raw flat solver and exclude `VIRTUAL_WALL`
transports, leaving existing `Dijkstra` behaviour unchanged.

This matters for shortest paths: a wall is an analysis partition boundary, not
an obstacle. The replacement interface must preserve each original cost-1
crossing adjacency; deleting the raw edge or retaining only the manual wall
transport would change the oracle.

## Required later query/oracle fields

The following are concrete minimum fields for the semantic upgrade. They are
not implemented here.

| Field | Purpose |
| --- | --- |
| `walkingMode` / raw-topology selector | Ensure flat and hierarchical tests choose raw walking, independently of manual walls. |
| `accountSkills` | Boosted skill levels plus total level, combat level, and quest points as needed by Java checks. |
| `completedQuests` | Exact quest-state set, or a richer quest-state map if non-finished states matter. |
| `varbitValues`, `varPlayerValues` | Immutable ID-to-value maps evaluated with the parsed operators. |
| `inventory`, `equipment`, `bankContents`, `runePouch` | Item quantities and variation equivalence for transport requirements. |
| `bankPickupPolicy` | Whether bank-obtainable items are allowed, and the cost/transition semantics of obtaining them. |
| `teleportPolicy` | Enabled types, consumable/non-consumable policy, unlocked policy, and any account-specific item setting. |
| `targetWildernessState` | The Java target exception used while deciding whether a walking boundary may be crossed. |
| `canUseGlobalTeleport(tile,state)` | Explicit broad-teleport eligibility, including wilderness/sailing/other location state. |
| `transportAvailability(transport,state)` | Single shared predicate used by flat and hierarchical edges. |
| `logicalHubId` and `hubUseState` | Required only when one-use hub semantics are intentionally enabled. |
| `transportActionId` / `familyId` | Separates logical actions from expanded origin-destination edges and supports later cost-matrix audits. |
| `transportBaseCost`, `queryPenalty`, `consumable` | Keeps static walking distances separate from query-dependent transport costs and resource changes. |

## Recommended milestone sequencing

1. **H0/H1:** Freeze the raw-oracle boundary and load/validate KaHIP ownership.
   Keep the existing manual-wall `Dijkstra` untouched. Do not regenerate
   partitions or alter generated data.
2. **H2:** Discover deduplicated terminals, gateways, and leaf-local exact
   metric closures. Keep separator tiles concrete and preserve all raw
   leaf-separator edges.
3. **H3:** Add a separate raw flat Dijkstra using `walkingNeighborsRaw`, the
   reachable tile scope, current type filtering, current penalties, current
   origin-less global behaviour, and the current free `banked` toggle.
4. **H4:** Build the exact hierarchy against that oracle. Keep all hub/network
   transports expanded. Defer `usedHubMask`, requirement evaluation, bank item
   transitions, wilderness eligibility, and any new capability state. Global
   teleports may be attached at query start and explicit arrival/state-change
   nodes only because that is exact under the current universal-availability
   model.
5. **H5/H6:** Pass the synthetic and real differential gates, including route
   reconstruction cost checks, before adding semantics.
6. **Semantic upgrade:** Extend the flat oracle and `Query` together with the
   fields above; add eligibility-boundary terminals and differential tests for
   bank-obtained items, requirements, wilderness, and global teleport access.
7. **Hub experiment:** Only after semantic parity, measure each logical family
   cost matrix and add one-use state or a proven decomposed hub representation.

Milestone 1 is deliberately incomplete with respect to Java production
semantics. That is a controlled limitation, not permission for the hierarchy
to invent capabilities or silently use parsed requirements.

## Commands used

Read-only inspection used `rg --files`, `rg -n`, and numbered `sed`/`nl`
extracts over `main-algorithm-plan.md`, `hierarchical-pathfinder-spec.md`, the
Haskell modules, and the relevant Java transport/pathfinder classes. No build,
partition run, generated-data regeneration, or runtime code change was made.
