module ShortestPath.Exact.ReferenceDijkstra
  ( ReferenceDijkstra(..)
  , findRouteReferenceDijkstra
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Wilderness
import ShortestPath.World

newtype ReferenceDijkstra = ReferenceDijkstra WorldTopology

data State = TileState Tile Bool | GlobalState GlobalCapability Bool
  deriving stock (Eq, Ord, Show)

data Prev = Prev State (Maybe RouteStep)
  deriving stock (Eq, Show)

findRouteReferenceDijkstra :: ReferenceDijkstra -> Query -> Route
findRouteReferenceDijkstra (ReferenceDijkstra topology) q = search (Set.singleton (0, start)) (Map.singleton start 0) Map.empty Set.empty 0
   where
    world = topologyWorld topology
    availability = prepareQueryTransports world q
    start = TileState (queryStart q) False
    targetTile = queryTarget q

    search queue best prev settled expanded =
      case Set.minView queue of
        Nothing -> Route maxBound expanded []
        Just ((cost, state), rest)
          | Set.member state settled -> search rest best prev settled expanded
          | stateTile state == Just targetTile -> Route cost expanded (reconstruct prev state)
          | otherwise ->
              let settled' = Set.insert state settled
                  (queue', best', prev') = foldl (relax cost state) (rest, best, prev) (neighbors state)
               in search queue' best' prev' settled' (expanded + 1)

    relax cost state (queue, best, prev) (next, stepCost, step) =
      let newCost = cost + stepCost
       in if newCost < Map.findWithDefault maxBound next best
            then (Set.insert (newCost, next) queue, Map.insert next newCost best, Map.insert next (Prev state step) prev)
            else (queue, best, prev)

    neighbors (GlobalState capability banked) =
      transportEdges banked (case capability of
        WildernessGlobals -> preparedWildernessGlobalTransports availability banked
        AllGlobals -> preparedGlobalTransports availability banked
        NoGlobals -> [])
    neighbors (TileState tile banked) = walk <> bank <> localTransports <> activation
     where
      walk =
        [ (TileState t banked, 1, Just (Walk t))
        | t <- walkingNeighborsRaw world tile
        , isWalkable (worldCollision world) t || usableOrigin banked t
        ]
      bank =
        [ (TileState tile True, 0, Just (Walk tile))
        | bankTransitionAvailable q (worldBanks world) banked tile
        ]
      localTransports =
        if allowTransports q
          then transportEdges banked (preparedLocalTransportsAt availability banked tile)
          else []
      activation =
        [ (GlobalState capability banked, 0, Nothing)
        | allowTransports q
        , let capability = globalCapabilityAt tile
        , capability /= NoGlobals
        ]

    transportEdges banked transports =
      [ (TileState dst banked, stepCost, Just step)
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
          Just (Prev p step) -> maybe id (:) step (go p)

    stateTile (TileState tile _) = Just tile
    stateTile _ = Nothing
