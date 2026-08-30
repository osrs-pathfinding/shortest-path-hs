# Inventory and Bank Modelling Plan

## Status

Deferred while raw routing performance is the active priority.

This plan describes query-time inventory and bank modelling for the Haskell
prototype and its local viewer. It must not expand the Dijkstra state into a
complete mutable inventory.

## Current gap

- `Query` controls transport types, penalties, and whether bank paths are
  enabled, but contains no inventory or bank contents.
- Transport item expressions are parsed from the source TSV files but ignored
  by every solver.
- Search state already contains the monotonic `banked :: Bool` flag.
- The viewer only exposes a general transport toggle.
- The current default consequently makes every otherwise-enabled global
  teleport available. This remains useful as a stress-test mode.

The Java implementation has separate transport-availability views before and
after banking. The Haskell prototype should use the same shape rather than put
inventory contents into every search state.

## Query model

Add query-time item configuration equivalent to:

```haskell
data ItemMode
  = IgnoreItemRequirements
  | EnforceItemRequirements

type ItemCounts = Map String Int

data Query = Query
  { ...
  , itemMode :: ItemMode
  , inventoryItems :: ItemCounts
  , bankItems :: ItemCounts
  , bankPathEnabled :: Bool
  }
```

`IgnoreItemRequirements` preserves the current all-items stress test.

Inventory entries initially use canonical source requirement names such as
`COINS`, `DRAMEN_STAFF`, and `LAW_RUNE`. Literal RuneLite item IDs and item
variation expansion are deferred.

## Search semantics

Keep the search state as:

```text
(node, banked)
```

Do not add inventory contents to the state.

- Before banking, item requirements use `inventoryItems`.
- After banking, positive requirements may use inventory and bank quantities.
- A quantity-zero absence requirement checks carried inventory only. Visiting
  a bank does not force the player to withdraw an unwanted item.
- Walking and transports preserve `banked`.
- A usable bank permits the monotonic transition `False -> True`.
- Items are not consumed. One available charge or resource quantity can enable
  repeated uses, matching the prototype's existing resource simplification.

Because banking only adds capabilities, the existing banking-state dominance
rule remains valid.

## Implementation

### 1. Requirement semantics

- Correct `parseItems` to match the Java grammar: AND between requirement
  groups and OR within one group.
- Add a pure item-expression evaluator.
- Cover AND, OR, positive quantities, zero quantities, insufficient quantities,
  and post-bank availability with focused tests.

### 2. Shared availability predicate

Add one shared predicate used by raw, normal, and hierarchical Dijkstra:

```haskell
transportAvailable :: Query -> Bool -> Transport -> Bool
```

It should combine transport-type enablement and item requirements. Fairy rings
also need the Java model's Dramen/Lunar staff requirement. The Lumbridge Elite
diary bypass belongs with later varbit/account modelling.

### 3. Explicit bank route step

Represent the state transition as `VisitBank Tile` instead of a zero-cost walk.
Emit a distinct `bank` route-step kind in JSON so route inspection shows where
bank-only capabilities become available.

### 4. HTTP API

Accept optional query fields such as:

```json
{
  "itemMode": "configured",
  "inventory": {"DRAMEN_STAFF": 1},
  "bank": {"COINS": 10000},
  "bankPathEnabled": true
}
```

Omitted fields retain current stress-test behaviour. Validate item names,
non-negative integer quantities, entry counts, and request size at the HTTP
boundary.

### 5. Viewer

- Add a `Configured` / `Ignore requirements` mode selector.
- Add compact inventory and bank editors accepting one `ITEM=quantity` entry
  per line.
- Persist the configuration in `localStorage`.
- Mark bank visits separately on the map and in route statistics.
- Validate malformed and duplicate entries before sending the request.

Autocomplete and literal item-ID lookup are deferred until the canonical-token
editor proves inadequate.

## Verification

Use the concrete route families from `shortest-path-tooling`:

- Catherby charter to Musa Point with coins only in the bank.
- Castle Wars to AKQ with a Dramen staff in inventory versus in the bank.
- A teleport item usable before the first bank while another requirement is
  bank-only.
- Banked items must not leak into an unbanked branch.
- Insufficient rune and coin quantities must reject transports.
- Raw and hierarchical solvers must return identical costs for each inventory
  configuration.
- `IgnoreItemRequirements` must retain cost 57 for the current benchmark route
  from `2448 3452 0` to `3285 3069 0`.

## Deferred scope

- Consumable depletion and charge tracking.
- Inventory capacity and withdrawal choices.
- Literal RuneLite item IDs and `ItemVariations` expansion.
- Separate equipment and rune-pouch modelling.
- Skills, quests, varbits, varplayers, and bank accessibility requirements.
- Bank pickup instructions listing the exact items used by the chosen route.

These features require a richer account model and should only be added after
the canonical requirement-token implementation is measured and validated.

