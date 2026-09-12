# Split routing into account, target, and search lifetimes

Refactor routing so that work is prepared at the lifetime at which it actually changes.

The desired model is:

```text
WorldStatic
    ↓
CompiledRoutingAccount
    ↓
PreparedTarget
    ↓
ForwardSearch
```

with these invalidation rules:

```text
world changes
    => rebuild everything

effective account routing capabilities change
    => rebuild CompiledRoutingAccount + PreparedTarget

target changes
    => rebuild PreparedTarget only

start changes
    => rerun ForwardSearch only
```

A moving player with the same account and destination must therefore reuse both the compiled account state and the complete target heuristic.

## 1. Introduce explicit preparation APIs

Restructure the current one-shot pathfinding entry point around functions conceptually like:

```haskell
compileRoutingAccount
  :: WorldStatic
  -> AccountBuild
  -> RoutingOptions
  -> CompiledRoutingAccount

prepareTarget
  :: WorldStatic
  -> CompiledRoutingAccount
  -> Tile
  -> PreparedTarget

searchPrepared
  :: WorldStatic
  -> CompiledRoutingAccount
  -> PreparedTarget
  -> Tile
  -> SearchOptions
  -> PathResult
```

Names can follow the existing codebase.

Keep a convenience one-shot API if useful:

```haskell
shortestPath world account start target =
  let account' = compileRoutingAccount world account ...
      target'  = prepareTarget world account' target
  in searchPrepared world account' target' start ...
```

But the reusable values must be first-class.

## 2. `CompiledRoutingAccount`

Move everything that depends on account capabilities but not on start/target into this value.

This should include the already-existing prepared account semantics such as:

```text
which transports are usable
carried vs banked transport availability
prepared requirement checks
bank/global capability information
account-static transport adjacency
any account-specific heuristic graph structure
```

Do not retain the raw `AccountBuild` as the thing used by the hot search where a prepared representation already exists.

### Cache by effective routing capability

Do not invalidate this just because an irrelevant account value changed.

Define an `EffectiveRoutingFingerprint` derived from the routing facts that actually affect the graph.

For example, if adding an unrelated bank item changes nothing about usable transports, the fingerprint should remain unchanged.

Conversely, anything that changes transport availability or cost must affect it.

This includes relevant routing options if they change the effective graph, for example enabled/disabled transport classes or transport penalties.

The rule should be:

```text
same effective routing fingerprint
    => safe to reuse CompiledRoutingAccount
```

Do not simply hash the entire raw `AccountBuild`.

## 3. `PreparedTarget`

Move all target-dependent heuristic work into an immutable prepared value.

It should contain everything needed by forward A* to evaluate the transport-aware heuristic for that target, including both Boolean lifecycle layers:

```text
pre-bank heuristic
post-bank heuristic
target attachment information
reverse labels / seed data needed by heuristicAtSearchNode
```

After `PreparedTarget` has been built, `searchPrepared` must not run:

```text
reverse Dijkstra
target attachment construction
seed-table construction
site-graph construction
```

again.

### Remove the start from heuristic preparation

This is an important architectural requirement.

If the current heuristic/site graph inserts both:

```text
start
target
```

as query-specific graph sites, remove the need for the start site.

The heuristic is a lower bound on distance **to the target**. It must be reusable from arbitrary start states.

Represent target attachment directly, for example by seeding the reverse problem with the target's attachment/component distances rather than mutating the graph with a query-specific start.

The required invariant is:

```text
prepareTarget account target
searchPrepared ... startA
searchPrepared ... startB
searchPrepared ... startC
```

all use the exact same `PreparedTarget`.

## 4. Keep genuinely start-dependent work in `searchPrepared`

Only work that actually depends on the current source belongs here.

Examples:

```text
initial A* state
initial g values
initial frontier
source-side carried global teleport alternatives
forward best/prev arrays
priority queue
bestBankCost
route reconstruction
```

In particular, carried global teleports that behave as source-side initial alternatives can be stored as capabilities in `CompiledRoutingAccount`, but their actual insertion into the frontier belongs to the forward query.

Do not accidentally move source semantics into `PreparedTarget`.

## 5. Cache simply at first

Do not build a complicated general-purpose cache.

Initially support the common interactive case with:

```text
current CompiledRoutingAccount
current PreparedTarget
```

Conceptually:

```text
if effectiveAccountFingerprint changed:
    account = compileRoutingAccount(...)
    target  = prepareTarget(account, targetTile)

else if targetTile changed:
    target = prepareTarget(account, targetTile)

result = searchPrepared(account, target, currentStart)
```

A one-entry cache is enough for RuneLite-style routing because normally:

```text
account stable
target stable
start changes continuously
```

A small target LRU can be added later if there is evidence it is useful.

Do not add persistent/disk caching at this stage.

## 6. Make invalidation explicit

Add tests for the lifetime boundaries.

### Start change

Given identical:

```text
world
account capabilities
target
```

changing only the start must:

```text
reuse CompiledRoutingAccount
reuse PreparedTarget
rerun ForwardSearch
```

and produce the same result as the existing one-shot pathfinder.

### Target change

Changing only target must:

```text
reuse CompiledRoutingAccount
rebuild PreparedTarget
```

### Relevant account change

A change that enables/disables a transport must:

```text
change EffectiveRoutingFingerprint
rebuild CompiledRoutingAccount
rebuild PreparedTarget
```

### Irrelevant account change

A raw account change that has no routing effect must:

```text
leave EffectiveRoutingFingerprint unchanged
reuse both prepared values
```

## 7. Preserve exact semantics

This is an architectural refactor, not an algorithm change.

For every existing benchmark:

```text
route reachability unchanged
route cost unchanged
exact A* semantics unchanged
heuristic values unchanged
bank/global dominance unchanged
```

The existing one-shot API and the new staged API should agree.

## 8. Add timing boundaries

Once the split exists, expose timings separately:

```text
account_prepare_ms
target_prepare_ms
forward_search_ms
```

Then define useful query modes:

```text
cold
    account preparation + target preparation + search

warm account
    target preparation + search

warm target
    search only
```

Do not muddle these into one setup timer.

## End state

The architecture should make this cheap and natural:

```haskell
account <- compileRoutingAccount world accountBuild options
target  <- prepareTarget world account destination

-- player moves:
path1 <- searchPrepared world account target start1 searchOptions
path2 <- searchPrepared world account target start2 searchOptions
path3 <- searchPrepared world account target start3 searchOptions
```

There must be no reverse-heuristic or account-graph reconstruction between `path1`, `path2`, and `path3`.

Commit this as a lifetime/API refactor. Do not combine it with weighted A*, heuristic-strength changes, or further forward-kernel optimisation.
