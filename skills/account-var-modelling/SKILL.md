---
name: model-osrs-account-semantics
description: >
  Model OSRS transport requirements semantically from RuneLite varbits and
  varplayers. Use when working through UNMODELLED account-vars, adding quest
  or permanent transport unlocks, extending early/mid/end/maxed AccountBuild
  profiles, or validating account-state coverage. Identify the underlying game
  concept rather than exposing raw numeric vars; reuse or extend the existing
  semantic account model; compile semantic state to raw game vars at the
  existing boundary; update realistic monotonic account builds; add focused
  tests; run the account-vars coverage report; and commit each coherent
  semantic concept or transport family separately. Do not guess unclear or
  transient game-state semantics.
---

# Account semantic modelling

Use this procedure whenever asked to model one or more `UNMODELLED` entries from `account-vars`.

The goal is **not** merely to make a numeric var requirement pass. The goal is to represent the underlying RuneScape account state semantically, and then compile that state to the raw varbit/varplayer values required by the imported transport data.

## For each account var

1. **Understand what it means**

   * Start from the entry in `account-vars`: semantic RuneLite name, raw ID, requirement type, use count, and transport source lines.
   * Inspect every relevant transport row to understand the comparison/operator and required value.
   * Search the checked-in RuneLite sources/constants for the varbit/varplayer name and its usage.
   * Determine the actual game concept: quest completion, permanent transport unlock, discovered destination, diary unlock, POH setting, cooldown/runtime state, etc.
   * Do not guess the meaning or magic value from the numeric ID alone.

2. **Reuse the existing semantic model where possible**

   * If the concept is already represented by `AccountBuild`, `Progression`, `PohBuild`, `RuntimeState`, quest completion, diaries, quetzal/platform unlocks, etc., extend that representation rather than inventing another mechanism.
   * Related vars should normally be represented as one coherent game concept. For example, a transport network with several destination-unlock bits should usually become a set of unlocked destinations, not several unrelated booleans.

3. **Add a semantic account field when necessary**

   * If there is no suitable existing representation, add the smallest semantic field/type which describes the RuneScape concept.
   * Prefer things such as:

```text
completed quest
Bool permanent unlock
Set Destination
enum progression/state
```

over exposing raw varbit numbers to callers.

4. **Compile semantics to raw game vars in one place**

   * The low-level requirement evaluator can continue consuming the numeric varbit/varplayer requirements imported from RuneLite.
   * Convert semantic account state into the corresponding `accountVarbits` / `accountVarPlayers` values when compiling an `AccountBuild`.
   * Raw IDs and magic values belong at this compilation boundary.
   * Do **not** scatter unexplained code such as:

```haskell
Map.insert 12345 7
```

through `early`, `mid`, `end`, or `maxed`.

The benchmark builds should say what the account has unlocked; the compiler should know which RuneLite vars that implies.

## Extend the benchmark account builds

For every new semantic capability, decide which of the four benchmark accounts possesses it:

```text
early
mid
end
maxed
```

Use the intended progression represented by the existing builds.

The profiles should remain plausible RuneScape accounts, and progression should normally be monotonic:

```text
early ⊆ mid ⊆ end ⊆ maxed
```

for permanent unlocks.

Examples:

```text
quest completed at mid:
    early = locked
    mid   = unlocked
    end   = unlocked
    maxed = unlocked

late-game permanent unlock:
    early = locked
    mid   = locked
    end   = unlocked
    maxed = unlocked
```

For quest-stage variables, model the stable state appropriate to each build rather than arbitrary intermediate quest states unless routing genuinely depends on an intermediate state.

For `maxed`, permanent account progression should normally be fully unlocked unless there is a real reason why a maxed account would not have that capability.

Do not force transient/user-choice state into progression. Cooldowns, spellbook/runtime state, POH configuration, etc. should use the corresponding runtime/configuration part of the account model.

## Validate each change

After modelling the var or coherent family:

1. Run the account-var coverage/report again.
2. Confirm the intended entries are no longer `UNMODELLED`.
3. Inspect the emitted values for all four profiles.
4. Check that the affected transports become available only for the intended profiles.
5. Add a focused test for the semantic mapping where useful, especially for:

   * bit masks;
   * enums/non-boolean values;
   * multiple related destination unlocks;
   * boundary progression (`early` false, `mid` true, etc.).
6. Run the normal test suite.

Do not change the transport TSV requirements merely to make the account model fit them unless investigation shows that the imported transport requirement itself is wrong.

## Scope and commits

Work on **one semantic concept or tightly related transport family at a time**.

Examples of sensible units:

```text
one quest unlock
all hot-air-balloon destination unlocks
one pendant destination family
one rowboat network
one minecart unlock system
```

After each semantic concept:

* validate it;
* commit it separately;
* then move to the next `UNMODELLED` entry/family.

Do not bundle a long list of unrelated account-var guesses into one commit.

If a var turns out to have unclear, transient, obsolete, or otherwise complicated semantics, **do not guess**. Record why it remains unmodelled and move on to the next cheap case.

## Completion criterion

The aim of this pass is:

> Every straightforward transport-related varbit/varplayer is explained by semantic account state and compiled systematically into the numeric state consumed by the requirement evaluator.

`UNMODELLED` should eventually mean **genuinely needs investigation**, not simply **has not yet been wired up**.

When I ask you to “model `<VAR>`” or “model the next account vars”, follow this procedure, make the semantic change, update the account builds, test it, and commit the completed semantic unit.
