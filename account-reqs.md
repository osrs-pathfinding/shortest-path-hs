# Account Requirements and Build Modelling Plan

Extend the pathfinding model so that transport availability accurately reflects different OSRS account builds.

The immediate goal is to support realistic benchmark profiles such as:

* early game;
* mid game;
* end game;
* maxed.

The implementation should support real transport requirements such as:

* skill levels;
* quest completion;
* varbits / varplayers;
* inventory items;
* equipped items;
* rune pouch contents;
* bank items.

A critical requirement is:

> Requirement handling must not make the A* hot loop expensive.

Most requirement work should happen once during query setup.

The real search and reverse heuristic should consume compact, pre-filtered transport structures rather than repeatedly evaluating quests, skills, item expressions, maps, sets, or strings during node expansion.

---

# 1. Separate account state from routing policy

Introduce an explicit account model.

Conceptually:

```haskell
data AccountBuild = AccountBuild
  { accountLevels      :: ...
  , accountQuests      :: ...
  , accountVarbits     :: ...
  , accountVarPlayers  :: ...

  , accountInventory   :: ...
  , accountEquipment   :: ...
  , accountRunePouch   :: ...
  , accountBank        :: ...
  }
```

`AccountBuild` should describe facts about the character.

Keep routing policy in `Query`.

Examples of query policy:

```text
start
target
enabled transport types
transport penalties
heuristic weight
whether bank paths are enabled
```

Do not treat these as account properties.

The distinction should be:

```text
AccountBuild:
    what this character can do

Query:
    what route we are asking for
    and how routing should be configured
```

---

# 2. Do not put static account requirements into A* state

Most account facts do not change during one route:

```text
Agility level
Magic level
quest completion
diary/unlock flags
varbits
varplayers
equipment
```

These should affect which transport edges exist.

They should NOT become dimensions of the A* state space.

For example, do not create states such as:

```text
(tile, agility>=70)
(tile, questXComplete)
(tile, hasDiaryY)
```

Those facts are fixed for the whole query.

Evaluate them during query setup.

---

# 3. Distinguish item access from transport lifecycle

The main capability change during a route is bank access.

Before visiting a bank:

```text
inventory
equipment
rune pouch
```

may be available.

After visiting a bank:

```text
inventory
equipment
rune pouch
bank
```

may be available.

Represent this concept explicitly.

For example:

```haskell
data ItemAccess
  = CarriedOnly
  | CarriedAndBank
```

Do not conflate this with the bank-global lifecycle.

These are separate concepts:

```text
Have bank items become available?

Is the bank-global teleport opportunity still available?
```

The heuristic may need several bank lifecycle phases, but requirement evaluation should consume a clear item-access capability.

For example:

```text
UnbankedGlobalAvailable
    -> CarriedOnly

UnbankedGlobalUnavailable
    -> CarriedOnly

Banked
    -> CarriedAndBank
```

---

# 4. Build one shared requirement evaluator

Implement one authoritative requirement evaluator used by all pathfinding algorithms.

Conceptually:

```haskell
requirementsSatisfied
  :: RequirementContext
  -> Transport
  -> Bool
```

with something like:

```haskell
data RequirementContext = RequirementContext
  { requirementAccount    :: AccountBuild
  , requirementItemAccess :: ItemAccess
  , requirementNowMinutes :: Int
  }
```

The generic evaluator should cover the requirement metadata already represented by transports.

Do not let:

```text
Raw Dijkstra
Tile A*
reverse heuristic
viewer
```

each implement slightly different availability semantics.

There should be one source of truth.

---

# 5. Skill requirements

For each transport skill requirement:

```text
required skill S >= level N
```

require:

```text
accountLevel[S] >= N
```

Missing skill data should fail in configured-account mode.

Do not silently assume an unknown skill requirement succeeds.

If the transport data contains pseudo-skills or special named levels, preserve those names and support them explicitly.

---

# 6. Quest requirements

For every required quest:

```text
quest ∈ accountCompletedQuests
```

must hold.

Missing quest completion should mean:

```text
not completed
```

in configured-account mode.

Do not infer quest completion from skill levels or other account properties.

---

# 7. Varbits and varplayers

Implement the existing parsed operators faithfully.

For example:

```text
=   equality
>   greater than
<   less than
&   bit/mask requirement
@   cooldown/timestamp requirement
```

Use explicit integer data from the account build.

Missing vars should fail the requirement rather than being treated as satisfied.

For cooldown-like requirements, do not read the wall clock directly inside requirement evaluation.

Pass a deterministic:

```text
nowMinutes
```

value in the requirement context.

This matters for repeatable benchmarks.

---

# 8. Improve item modelling

Support item availability from:

```text
inventory
equipment
rune pouch
bank
```

Build a unified item-count representation.

For example:

```haskell
availableItems
  :: AccountBuild
  -> ItemAccess
  -> ItemCounts
```

Then:

```text
CarriedOnly:
    inventory
    + equipment
    + rune pouch

CarriedAndBank:
    inventory
    + equipment
    + rune pouch
    + bank
```

Use the existing item-expression semantics where possible:

```text
AND
OR
quantity
alternative items
```

---

# 9. Do not model general item consumption yet

For this phase, item requirements are capability checks.

Do not attempt to track:

```text
number of teleport tablets remaining
number of charges remaining
coins consumed
runes consumed
```

as A* state.

That would multiply the state space dramatically.

Document the current simplification:

> If an account has the required resource, it may satisfy repeated transport checks unless a separate lifecycle rule explicitly prevents reuse.

One-shot global and hub semantics should remain separate lifecycle logic.

---

# 10. Support an explicit ignore-requirements mode

Keep a mode equivalent to the current broad transport stress test.

For example:

```haskell
data RequirementMode
  = IgnoreRequirements
  | ConfiguredRequirements AccountBuild
```

Do not define the maxed account as:

```text
IgnoreRequirements
```

A maxed account should still be a concrete account build with:

```text
high levels
quests complete
unlock vars
rich inventory
rich bank
```

This gives us two useful test cases:

```text
realistic maxed account

everything-enabled stress test
```

---

# 11. PERFORMANCE REQUIREMENT: preprocess availability once per query

This is a major design requirement.

Do not perform expensive generic requirement checks inside the A* expansion loop.

Bad hot-loop behaviour would look like:

```haskell
for every expanded tile:
    for every transport:
        look up skill name in Map
        search quest Set
        interpret item expression
        inspect varbits
        inspect bank items
```

That is unacceptable.

Instead, build a compact query-specific transport view once.

For example:

```haskell
data QueryTransportAvailability = QueryTransportAvailability
  { carriedLocal   :: ...
  , bankedLocal    :: ...
  , carriedGlobals :: ...
  , bankedGlobals  :: ...
  }
```

Exact representation can differ, but the search should consume already-filtered structures.

At query setup:

```text
all transports
    |
    | requirements/account evaluation
    v
query-specific available transport sets
    |
    v
real A*
reverse heuristic
```

The hot loop should mostly do:

```text
index adjacency
iterate compact edges
add cost
relax state
```

not evaluate account semantics.

---

# 12. Prefer compact indexed structures in hot paths

The implementation should distinguish between:

```text
configuration representation
```

and:

```text
runtime search representation
```

It is fine for account configuration to use readable structures such as:

```text
Map
Set
strings
JSON
```

But after query preparation, convert the relevant data into efficient indexed representations.

Prefer things such as:

```text
integer transport IDs
vectors
flat arrays
bitsets
pre-filtered adjacency
compact item IDs
```

where appropriate.

Avoid in the hot loop:

```text
String comparisons
Map lookups
Set membership
parsing requirement expressions
allocating temporary lists
reconstructing item pools
```

Do not optimise blindly, but design the boundary so these optimisations are possible.

---

# 13. Precompute static requirement eligibility separately

Many requirements do not depend on banking.

For a particular account:

```text
skills
quests
varbits
varplayers
transport type configuration
```

are fixed.

So use a staged model:

```text
all transports
      |
      | account-static requirements
      v
account-eligible transports
      |
      +-- carried items ------> pre-bank available
      |
      +-- carried + bank -----> post-bank available
```

This keeps requirement logic clear and can reduce repeated work.

For benchmark suites using the same account profile for hundreds of routes, consider caching account-static eligibility across queries.

Do not prematurely cache target/query-dependent information.

---

# 14. Real search and heuristic must share availability

This is now a correctness-critical invariant.

The real A* and reverse heuristic must consume the same account-availability model.

In particular:

> Every real transport edge available to the forward search must also exist in the relaxed heuristic graph in the corresponding capability state.

The heuristic may contain EXTRA optimistic edges.

It must not accidentally omit a real edge.

This matters because the current heuristic uses:

```text
no relaxed path to target
```

as proof that the real state can be pruned.

That proof is only sound if the heuristic graph contains every real continuation.

---

# 15. Add requirement explanations outside the hot loop

Provide a diagnostic API which can explain why a transport is unavailable.

For example:

```haskell
data TransportAvailability
  = Available
  | MissingItems ...
  | MissingSkills ...
  | MissingQuests ...
  | FailedVarRequirements ...
  | TransportTypeDisabled
```

This does NOT need to be used by the search hot loop.

The benchmark viewer/debug tooling can use it.

For example:

```text
Fairy Ring XYZ

Unavailable for Early profile:

    Fairytale II not completed
    Dramen staff unavailable
```

This will be useful when inspecting different account builds.

---

# 16. Define four realistic benchmark account profiles

Create checked-in profiles:

```text
benchmarks/accounts/early.json
benchmarks/accounts/mid.json
benchmarks/accounts/end.json
benchmarks/accounts/maxed.json
```

They should be explicit character descriptions, not percentages of available transports.

## Early

Aim for:

```text
mostly walking
basic local transports
few quest unlocks
few teleport systems
low/moderate skills
small bank teleport inventory
```

## Mid

Aim for a plausible developed account with:

```text
common jewellery teleports
fairy rings
some spirit trees
moderate agility
many standard quest transports
useful inventory
useful bank
```

This is likely to be one of the most interesting benchmark profiles.

## End

Aim for:

```text
high levels
most quests
most common hubs
most shortcuts
rich teleport inventory/bank
```

but leave some late/end-game requirements unavailable.

## Maxed

Aim for a plausible fully developed account:

```text
99 skills where appropriate
all normal quests
major diary/unlock vars
rich inventory
rich bank
all ordinary permanent transport systems
```

This is still NOT `IgnoreRequirements`.

---

# 17. Add profile validation tooling

Account profiles will otherwise be hard to reason about.

Provide a command such as:

```text
account-profile validate benchmarks/accounts/mid.json
```

Report:

```text
transport edges available
global teleports available
local transports available

requirements blocking transports:
    items
    skills
    quests
    vars

hub families:
    Fairy Ring      available
    Spirit Tree     available
    Quetzal         unavailable
    ...
```

Also make it possible to compare profiles:

```text
early -> mid
mid -> end
end -> maxed
```

and show newly available transports/families.

This is useful for catching nonsense account configurations.

---

# 18. Account profiles in the benchmark suite

The benchmark matrix should become:

```text
~750 fixed routes
x early
x mid
x end
x maxed
```

For each benchmark run, record useful account graph context:

```text
available local transport count
available global count
available bank-only transport count
available hub families
```

This helps explain performance differences.

For example:

```text
Maxed expanded more states than End

possible explanation:
    2x as many plausible transport alternatives
```

---

# 19. Account profiles in the viewer

Extend the existing route viewer so a benchmark route can be loaded with:

```text
early
mid
end
maxed
```

or another explicit profile.

The viewer should use exactly the same account model as the search.

When inspecting transports, allow debugging of:

```text
available
unavailable
reason unavailable
```

Do not build a full account editor initially.

Checked-in JSON profiles are sufficient.

---

# 20. Generic requirement coverage report

Scan the entire transport dataset and report usage of:

```text
item requirements
skill requirements
quest requirements
varbits
varplayers
Wilderness restrictions
consumable flags
other special metadata
```

Then report which requirement forms the generic evaluator supports.

Aim for:

```text
generic parsed requirements understood: 100%
```

Anything not understood should be reported explicitly.

Do not silently ignore unknown requirement types.

---

# 21. Keep special game rules separate

Do not try to solve every OSRS special case in this first implementation.

Examples which may need dedicated modelling later:

```text
Wilderness teleport levels
POH furniture/configuration
planted spirit-tree locations
sailing/boat location
balloon-specific state
special teleport cooldowns
regional restrictions
```

Keep the generic requirement evaluator clean.

Add special capability predicates separately when required.

Do not accumulate unrelated special cases inside one giant:

```haskell
transportAvailable
```

function.

---

# 22. Correctness tests

Add unit tests for every generic requirement form.

## Skills

```text
below requirement
equal requirement
above requirement
missing skill
```

## Quests

```text
completed
not completed
missing
```

## Vars

```text
=
>
<
&
@
missing value
```

## Items

```text
single item
AND
OR
quantity
insufficient quantity
inventory
equipment
rune pouch
bank-only
```

## Capability phase

Check:

```text
pre-bank transport unavailable
post-bank transport available
```

where appropriate.

---

# 23. Solver parity tests

The strongest architectural test should be:

> For the same query/account capability state, all solvers consume the same precomputed transport availability.

Avoid testing duplicated implementations if possible.

Instead, architect the code so:

```text
Raw Dijkstra
Tile A*
reverse heuristic
```

all receive the same prepared transport structures.

Then run differential route-cost testing across the four profiles.

---

# 24. Performance tests

Requirement modelling must come with performance measurements.

Measure separately:

```text
account/static requirement preparation
query-specific transport preparation
reverse heuristic
real A* search
total query
```

For the hot loop, benchmark:

```text
transport neighbour iteration cost
```

before and after the change.

The intended result is:

> More accurate account semantics should mostly increase query preparation work, not per-node search cost.

If A* expansion throughput drops substantially because of requirement handling, redesign the runtime representation.

---

# 25. Benchmark repeated-profile optimisation

The benchmark suite will run hundreds of routes against the same account profile.

Exploit this where safe.

Potentially split preparation into:

```text
AccountPrepared
    skills/quests/vars resolved
    account-static eligible transports

QueryPrepared
    enabled transport policy
    penalties
    carried/banked item views
    query-specific structures
```

This lets:

```text
750 routes x Mid
```

reuse the expensive account-static preparation.

Do not make this optimisation mandatory for the first correct implementation, but design the API so it can be added cleanly.

---

# 26. Suggested implementation sequence

Implement in this order:

1. Introduce `AccountBuild`.
2. Move existing inventory/bank configuration into it.
3. Implement pure generic requirement evaluation.
4. Implement skills.
5. Implement quests.
6. Implement varbits/varplayers.
7. Extend item access to inventory/equipment/rune pouch/bank.
8. Introduce explicit `ItemAccess`.
9. Precompute account-static eligible transports.
10. Build compact pre-bank/post-bank transport structures.
11. Make Tile A* consume those structures.
12. Make reverse heuristic consume exactly the same availability model.
13. Make raw/reference Dijkstra consume the same structures.
14. Add diagnostic availability explanations.
15. Build Early/Mid/End/Maxed profiles.
16. Add profile validator.
17. Run differential correctness tests across profiles.
18. Benchmark hot-loop throughput and query-setup cost.
19. Integrate account selection into benchmark corpus/viewer.
20. Only then tackle special cases such as Wilderness rules.

---

# Core invariants

Keep these explicit in the implementation.

## Account facts are mostly static

```text
skills
quests
vars
```

filter the graph.

They do not expand the routefinding state space.

## Search state only tracks route-changing capabilities

Only facts that genuinely change during the route should appear in A* lifecycle state.

## Real search and heuristic share transport availability

If the forward search can use an edge, the relaxed heuristic must also know about it.

## Requirement evaluation is not a hot-loop operation

The hot loop should consume compact, precomputed transport adjacency.

## Configuration can be rich; runtime representation should be simple

Readable JSON/Maps/Sets are fine at configuration boundaries.

The search itself should prefer:

```text
integer IDs
vectors
flat adjacency
bitsets
pre-filtered edges
```

where profiling supports it.

## Do not trade search throughput for modelling convenience

A more accurate account model is valuable, but transport requirement checking should not become a major per-expansion cost.

The desired architecture is:

```text
rich account model
       |
       | preprocess
       v
compact query-specific graph
       |
       v
fast A*
```

rather than:

```text
rich account model
       |
       v
interpret requirements repeatedly inside A*
```

