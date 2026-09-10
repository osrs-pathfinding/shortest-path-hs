# Semantic model

## Account and transport requirements

Account configuration has one normal compilation path:

```text
benchmark profile / future account importer
                  |
                  v
             AccountSpec
       semantic OSRS account facts
                  |
        compileAccount at query time
                  |
                  v
             AccountState
 skills, quests, items, varbits, varplayers,
 runtime values and compiled POH capabilities
                  |
                  v
          RequirementContext
       item access + current time
                  |
                  v
       transportAvailability
                  |
                  v
       prepareQueryTransports
            /             \
   Raw Dijkstra         Tile A*
```

`ShortestPath.AccountSemantics` owns semantic progression types and the
semantic-to-RuneLite mapping. `ShortestPath.BenchmarkProfiles` only defines
the Early/Mid/End/Maxed fixtures. Numeric game state is derived during
`compileAccount`; benchmark profiles do not independently assign known
semantic varbits or varplayers.

`RawGameState` is an explicit escape hatch for exceptional raw overrides.
An override may repeat a derived value, but a contradictory value produces an
`AccountCompileError` rather than silently taking precedence.

`AccountState` is the canonical concrete state consumed downstream.
`RequirementContext` adds query-specific carried-versus-bank access and the
current time. `transportAvailability` is the authoritative interpreter for
items, skills, quests, varbits, varplayers and the narrowly documented fairy
ring/POH rules. Routing implementations consume transports already filtered by
`prepareQueryTransports`; they do not reinterpret account semantics.

The Maxed benchmark fixture currently retains the historical compatibility
rule that its bank includes every item named by the loaded transport corpus.
Changing that workload dependency is a separate semantic change and requires
a fresh route-result comparison.

## Tile A* — Important Semantic Optimisations

The basic algorithm is still A* over real OSRS tiles.

Walking follows the real collision map. Transports use the real directed transport edges and costs.

The main improvements are in how we build and use the heuristic. The heuristic is not just geometric distance to the target. It models important parts of the transport system while deliberately relaxing other parts.

1. **The heuristic understands transports, not just geography.**

   We run a reverse shortest-path search from the target over transport-relevant sites.

   This gives each useful transport site a lower-bound cost to the target.

   For a real tile `x`, the heuristic is then roughly:

   ```text
   min over useful sites s in this component:
       Chebyshev(x, s) + costFromSiteToTarget(s)
   ```

   This means A* can understand that walking away from the target towards a good teleport may be the right thing to do.

2. **Relaxed walking is only allowed inside the same natural walking component.**

   We do not pretend that the whole coordinate plane is freely walkable.

   If two sites are in the same connected walking component, the heuristic may replace their real walking distance with cheap Chebyshev distance.

   If they are in different components, the heuristic cannot walk between them. It must use an explicit transport.

   This keeps the heuristic much stronger than plain geometric distance while remaining optimistic.

3. **Initial global teleports are resolved before normal heuristic-guided search.**

   Globals that are immediately available at the start are special.

   The initial A* frontier contains:

   ```text
   start without using a global
   global destination A
   global destination B
   ...
   ```

   The heuristic graph itself does not contain these initial global edges.

   This is important. Otherwise the heuristic could incorrectly assume that an initial teleport remains available later in the route.

   Once an ordinary search state is in the queue, the initial global decision has already been made.

4. **The bank-global opportunity is one-shot.**

   Before visiting a bank, the search can still choose to use a bank-enabled global teleport.

   At the first useful bank there are two choices:

   ```text
   do not use a global:
       unbanked @ bank -> banked @ same bank

   use a global:
       unbanked @ bank -> banked @ teleport destination
   ```

   Once the state is banked, there is no further bank-global opportunity.

   This prevents impossible relaxed routes such as:

   ```text
   bank
   -> teleport
   -> another bank
   -> teleport again
   -> another bank
   -> ...
   ```

   Banked states can still use bank-held items for ordinary transports.

5. **We dynamically recognise when another route to a bank is dominated.**

   During the real search we remember the cheapest concrete cost found so far to any bank:

   ```text
   bestBankCost
   ```

   Suppose we have already found a bank for cost 20.

   An unbanked branch that already costs 25 cannot possibly produce a cheaper bank-global route. Any future walk to a bank would only increase its cost.

   We therefore treat the bank-global opportunity as dominated for that branch.

   We use the stronger post-bank-global heuristic as an additional lower bound:

   ```text
   max(unbanked heuristic, banked heuristic)
   ```

   This removes large search "mushrooms" around inferior banks.

   We do not discard the whole state. The physical location may still be useful for walking, local transports, or reaching bank-held items.

6. **When a better bank route is found, queued states are re-evaluated lazily.**

   `bestBankCost` can improve during the search.

   A state may have entered the priority queue before the cheaper bank was discovered.

   When such a state is popped, we recompute its effective heuristic.

   If the newly discovered bank dominance makes its priority worse, we put it back into the queue with the stronger priority instead of expanding it immediately.

   This lets information discovered by the forward search dynamically strengthen the heuristic.

7. **States which cannot reach the target even in the relaxed graph are pruned.**

   Previously, a component with no finite heuristic value effectively received:

   ```text
   h = 0
   ```

   This was very bad. A* could flood an entire dead-end component because it looked artificially attractive.

   The heuristic now returns "unreachable" instead.

   Such a state is never added to the queue.

   This applies when:

   * the tile is outside the retained reachable world; or
   * its component/lifecycle state has no finite reverse path to the target.

   Because the heuristic graph is a relaxation of the real graph, failure to reach the target in the relaxed graph proves that the real continuation is also impossible.

8. **Blocked transport tiles can belong to the surrounding walking component.**

   Some useful transport objects, such as fairy rings, sit on blocked tiles.

   Looking only at the collision state of the transport tile would incorrectly make them disconnected from the heuristic.

   `heuristicComponent` first checks the tile itself. If it is blocked, it checks the adjacent tiles reachable under the real walking/transport-origin rules.

   This lets a blocked fairy-ring tile inherit the natural component of the walkable tiles around it.

   The heuristic therefore sees the fairy ring as a valid walking destination even though the object tile itself is blocked.

9. **The world is restricted to components that are actually reachable.**

   Natural walking components are computed once.

   Components which cannot be reached from the main reachable world are not retained as ordinary search space.

   This is both a practical optimisation and a useful invariant: we do not spend time searching disconnected map data that a real route cannot enter.

10. **The real search and heuristic deliberately have different walking models.**

    The real A* uses:

    ```text
    real collision-aware walking
    + real transports
    ```

    The heuristic uses:

    ```text
    collision-free Chebyshev walking
    inside each natural component
    + relaxed transport graph
    ```

    This separation is intentional.

    The heuristic should be cheap and optimistic, but it must preserve the important topology and lifecycle rules that prevent impossible cheap transport cycles.

## The main design principle

The most important lesson is that the heuristic should not try to reproduce the whole real game state.

Instead, we model the few semantic details whose relaxation causes very bad search behaviour.

In particular:

```text
keep:
    natural walking-component topology
    one-shot initial-global semantics
    one-shot bank-global semantics
    bank-item availability
    proven unreachable regions
    dynamic dominance of inferior bank-global routes

relax:
    exact collision distance inside a component
    many smaller local details
```

The aim is to prevent impossible cheap route patterns without exploding the heuristic state space.

This has been much more effective than simply increasing the A* heuristic weight. A larger weight cannot fix a heuristic which thinks an impossible repeated teleport route is cheap.
