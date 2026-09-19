# Semantic game-state profiles

Transport TSV files retain RuneLite's numeric requirements. Benchmark profiles use the generated semantic constants instead:

```text
3741=1 -> DRAGONSLAYER_CRANDOR_FOUND_SECRET_DOOR -> Dragon Slayer completion state
```

The generated routing-relevant registry is in `data/gamevars/transport-gamevars.tsv`. Its source RuneLite revision is recorded in `data/gamevars/runelite-revision.txt`. Haskell constants and reverse name lookups are generated under `src/ShortestPath/GameVars`.

Regenerate the registry from a pinned RuneLite checkout or source files:

```bash
node tools/account-profiles/generate-gamevars.js \
  --varbit /path/to/runelite-api/src/main/java/net/runelite/api/gameval/VarbitID.java \
  --varplayer /path/to/runelite-api/src/main/java/net/runelite/api/gameval/VarPlayerID.java \
  --revision RUNELITE_COMMIT
```

The importer scans every transport TSV, reports every referenced variable and fails when a routing-relevant ID has no RuneLite name. Varbit and varplayer namespaces remain distinct.

Profile source describes state semantically, for example:

```haskell
progressionQuests = Set.singleton "Dragon Slayer I"
```

`compileAccount` maps `AccountSpec` to the canonical concrete `AccountState`,
including efficient typed varbit and varplayer maps. The mappings live in
`ShortestPath.AccountSemantics`; `ShortestPath.BenchmarkProfiles` contains only
the benchmark fixtures. `account-profile vars` prints the semantic name,
numeric identity, requirements, transport sources and each profile's compiled
value. `UNMODELLED` means the compiled state has no value for the variable; it
is not silently treated as zero.

Exceptional raw state belongs in `RawGameState`. Raw overrides that contradict
semantic derivation are rejected with `AccountCompileError`.

Profile state is compiled from progression, diary completion, permanent unlocks, POH configuration, carried/banked items and runtime state. Do not make `maxed` mean that every observed variable requirement is enabled: some variables are mutually exclusive, transient or configuration-dependent.

For manual verification, the authoritative RuneLite lookups are:

```bash
rg ' = <ID>;' runelite-api/src/main/java/net/runelite/api/gameval/VarbitID.java
rg ' = <ID>;' runelite-api/src/main/java/net/runelite/api/gameval/VarPlayerID.java
```

## Deliberately unmodelled transport variables

The following audit entries are intentionally absent from the static benchmark
profiles. They must not be assigned a convenient value merely to make a
transport available.

| Variable | Classification | Why it remains unmodelled |
| --- | --- | --- |
| `KARAM_DUNGEON_ENTRYFEE` | Runtime state | Records whether the Brimhaven Dungeon entrance fee has already been paid for the current visit. The transport data already has coin-paying alternatives; making this a permanent unlock would incorrectly make later entry free without modelling the payment transition. |
| `VEOS_MEMOIR_CHARGES` | Runtime state | Counts consumable Kharedst's memoirs/Book of the dead charges. Availability depends on the account's current charged-item state and teleport consumption, not permanent progression. |
| `TAPOYAUIK_RUINS_FAILED_WALLSLIDE` | Runtime state / needs investigation | GPS uses this failure-named flag to unlock the Pendant of ates Kastori destination. Confirm whether the flag is a durable discovery unlock or transient obstacle state before adding it to progression. |
| `TAPOYAUIK_FAILED_STEPPING_STONES` | Runtime state / needs investigation | GPS uses this failure-named flag to unlock the Pendant of ates Nemus Retreat destination. Its persistence and exact unlock semantics need confirmation. |
| `LEAGUE_COMBAT_MASTERY_PATHS` | Special mode | Belongs to seasonal Leagues state. The normal early/mid/end/maxed profiles intentionally describe main-game accounts and must not enable League-only transports. |
| `HAUNTED` | Needs investigation | The exact `=3` requirement gates entry to the Killerwatt plane. The generated name and apparent quest association do not establish that ordinary quest completion has this stable value, so no profile value is inferred. |

Revisit an entry only when its authoritative persistence and value semantics are
known. Runtime entries should be represented in `RuntimeState` (including any
required consumption transition); durable discoveries should become semantic
permanent unlocks; special-mode state should use a separate profile rather than
leaking into the normal progression profiles.
