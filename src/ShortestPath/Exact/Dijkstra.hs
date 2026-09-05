module ShortestPath.Exact.Dijkstra
  ( Dijkstra(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

newtype Dijkstra = Dijkstra World

data State = State Tile Bool
  deriving stock (Eq, Ord, Show)

data Prev = Prev State RouteStep
  deriving stock (Eq, Show)

instance RouteFinder Dijkstra where
  routeName _ = "dijkstra"
  findRoute (Dijkstra world) q = search (Set.singleton (0, start)) (Map.singleton start 0) Map.empty Set.empty 0
   where
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
        | t <- walkingNeighbors world tile
        , isWalkable (worldCollision world) t || usableOrigin banked t
        ]
      bank =
        [ (State tile True, 0, Walk tile)
        | queryBankPathEnabled
        , not banked
        , Set.member tile (worldBanks world)
        ]
      localTransports = if allowTransports q then transportEdges banked (localAt banked tile) else []
      -- Walking before a broad-origin teleport is dominated by using it immediately.
      initialGlobalTransports =
        [ edge
        | allowTransports q
        , not banked
        , tile == queryStart q
        , edge <- transportEdges False (globalAt False)
        ]
      bankGlobalTransports =
        [ (State dst True, stepCost, step)
        | allowTransports q
        , queryBankPathEnabled
        , not banked
        , Set.member tile (worldBanks world)
        , (State dst _, stepCost, step) <- transportEdges True (globalAt True)
        ]
      queryBankPathEnabled = bankPathEnabled q

    transportEdges banked transports =
      [ (State dst banked, duration t + Map.findWithDefault 0 (transportType t) (transportPenalties q), UseTransport (label t) dst)
      | t <- transports
      , Just dst <- [destination t]
      ]

    localAt banked tile = Map.findWithDefault [] tile (if banked then bankedLocalTransports availability else carriedLocalTransports availability)
    globalAt banked = if banked then bankedGlobalTransports availability else carriedGlobalTransports availability
    usableOrigin banked tile = allowTransports q && not (null (localAt banked tile))

    label t = if null (displayInfo t) then transportType t else displayInfo t

    reconstruct prev state =
      reverse (go state)
     where
      go s =
        case Map.lookup s prev of
          Nothing -> []
          Just (Prev p step) -> step : go p
