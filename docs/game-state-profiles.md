# Semantic game-state profiles

Transport TSV files retain RuneLite's numeric requirements. Benchmark profiles use the generated semantic constants instead:

```text
3741=1 -> DRAGONSLAYER_CRANDOR_FOUND_SECRET_DOOR -> Dragon Slayer completion state
```

The generated routing-relevant registry is in `data/gamevars/transport-gamevars.tsv`. Its source RuneLite revision is recorded in `data/gamevars/runelite-revision.txt`. Haskell constants and reverse name lookups are generated under `src/ShortestPath/GameVars`.

Regenerate the registry from a pinned RuneLite checkout or source files:

```bash
node scripts/generate-gamevars.js \
  --varbit /path/to/runelite-api/src/main/java/net/runelite/api/gameval/VarbitID.java \
  --varplayer /path/to/runelite-api/src/main/java/net/runelite/api/gameval/VarPlayerID.java \
  --revision RUNELITE_COMMIT
```

The importer scans every transport TSV, reports every referenced variable and fails when a routing-relevant ID has no RuneLite name. Varbit and varplayer namespaces remain distinct.

Profile source should describe state semantically, for example:

```haskell
Map.singleton VB.dragonslayerCrandorFoundSecretDoor 1
```

The final `AccountBuild` still contains efficient typed maps. `account-profile vars` prints the semantic name, numeric identity, requirements, transport sources and each profile's compiled value. `UNMODELLED` means the profile has no value for the variable; it is not silently treated as zero.

Profile state is compiled from progression, diary completion, permanent unlocks, explicit configuration and runtime state. Do not make `maxed` mean that every observed transport requirement is enabled: some variables are mutually exclusive, transient or configuration-dependent.

For manual verification, the authoritative RuneLite lookups are:

```bash
rg ' = <ID>;' runelite-api/src/main/java/net/runelite/api/gameval/VarbitID.java
rg ' = <ID>;' runelite-api/src/main/java/net/runelite/api/gameval/VarPlayerID.java
```
