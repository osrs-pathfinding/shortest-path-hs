module ShortestPath.Exact.ReferenceDijkstra
  ( ReferenceDijkstra(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.World

newtype ReferenceDijkstra = ReferenceDijkstra WorldTopology

data State = State Tile Bool
  deriving stock (Eq, Ord, Show)

data Prev = Prev State RouteStep
  deriving stock (Eq, Show)

instance RouteFinder ReferenceDijkstra where
  routeName _ = "reference-dijkstra"
  findRoute (ReferenceDijkstra topology) q = search (Set.singleton (0, start)) (Map.singleton start 0) Map.empty Set.empty 0
   where
    world = topologyWorld topology
    availability = prepareQueryTransports world q
    start = State (queryStart q) False
    targetTile = queryTarget q

    search queue best prev settled expanded =
      case Set.minView queue of
        Nothing -> Route maxBound expanded []
        Just ((cost, state@(State tile _banked)), rest)
          | Set.member state settled -> search rest best prev settled expanded
          | tile == targetTile -> Route cost expanded (reconstruct prev state)
          | otherwise ->
              let settled' = Set.insert state settled
                  (queue', best', prev') = foldl (relax cost state) (rest, best, prev) (neighbors state)
               in search queue' best' prev' settled' (expanded + 1)

    relax cost state (queue, best, prev) (next, stepCost, step) =
      let newCost = cost + stepCost
       in if newCost < Map.findWithDefault maxBound next best
            then (Set.insert (newCost, next) queue, Map.insert next newCost best, Map.insert next (Prev state step) prev)
            else (queue, best, prev)

    neighbors (State tile banked) =
      walk <> bank <> localTransports <> initialGlobalTransports <> bankGlobalTransports
     where
      walk =
        [ (State t banked, 1, Walk t)
        | t <- walkingNeighborsRaw world tile
        , isWalkable (worldCollision world) t || usableOrigin banked t
        ]
      bank =
        [ (State tile True, 0, Walk tile)
        | bankTransitionAvailable q (worldBanks world) banked tile
        ]
      localTransports =
        if allowTransports q
          then transportEdges banked (preparedLocalTransportsAt availability banked tile)
          else []
      -- Walking before a broad-origin teleport is dominated by using it immediately.
      initialGlobalTransports =
        [ edge
        | allowTransports q
        , not banked
        , tile == queryStart q
        , edge <- transportEdges False (preparedGlobalTransports availability False)
        ]
      bankGlobalTransports =
        [ (State dst True, stepCost, step)
        | allowTransports q
        , bankTransitionAvailable q (worldBanks world) banked tile
        , (State dst _, stepCost, step) <- transportEdges True (preparedGlobalTransports availability True)
        ]

    transportEdges banked transports =
      [ (State dst banked, stepCost, step)
      | t <- transports
      , Just (dst, stepCost, step) <- [preparedTransport q t]
      ]

    usableOrigin banked tile = allowTransports q && not (null (preparedLocalTransportsAt availability banked tile))

    reconstruct prev state =
      reverse (go state)
     where
      go s =
        case Map.lookup s prev of
          Nothing -> []
          Just (Prev p step) -> step : go p
