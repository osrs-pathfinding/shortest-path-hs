{-# LANGUAGE MonoLocalBinds #-}

module ShortestPath.Heuristic.Region
  ( RegionGraph
  , RegionValues
  , buildRegionGraph
  , regionGraphEdgeCount
  , regionGraphNodeCount
  , regionValues
  , tileLowerBound
  , leafLowerBounds
  , globalLowerBound
  ) where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Min as PQueue
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable

import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Types
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

data RegionGraph = RegionGraph
  { reverseEdges :: Boxed.Vector [(Int, Int, Maybe String)]
  , tileNodes :: IntMap.IntMap Int
  , leafNodes :: Map.Map LeafId Int
  , globalNode :: Int
  , regionGraphEdgeCount :: Int
  }

data RegionValues = RegionValues
  { graph :: RegionGraph
  , distances :: Vector.Vector Int
  }

regionGraphNodeCount :: RegionGraph -> Int
regionGraphNodeCount = Boxed.length . reverseEdges

buildRegionGraph :: World -> Hierarchy -> RegionGraph
buildRegionGraph world hierarchy =
  RegionGraph reversed tileIndex leafIndex globalId (length edges)
 where
  spatialTiles = Set.toAscList (Set.unions
    [ Map.keysSet (terminalLeaf hierarchy)
    , Map.keysSet (hierarchySeparatorNodes hierarchy)
    , transportEndpoints world
    ])
  tileIndex = IntMap.fromList (zip (map unTile spatialTiles) [0 ..])
  tileCount = length spatialTiles
  leaves = Map.keys (leafOverlays hierarchy)
  leafIndex = Map.fromList (zip leaves [tileCount ..])
  globalId = tileCount + length leaves
  nodeCount = globalId + 1

  leafEdges = concatMap overlayEdges (Map.toList (leafOverlays hierarchy))
  overlayEdges (leaf, overlay) =
    case Map.lookup leaf leafIndex of
      Nothing -> []
      Just hub -> concatMap (terminalEdges hub) (Map.toList (leafTerminalAdjacency overlay))
  terminalEdges hub (tile, adjacent) =
    case (nodeFor tile, minimumMaybe (Map.elems adjacent)) of
      (Just terminal, Just lowerBound) -> [(terminal, hub, lowerBound, Nothing), (hub, terminal, 0, Nothing)]
      _ -> []

  separatorEdges = concatMap separatorConnections (Map.keys (hierarchySeparatorNodes hierarchy))
  separatorConnections tile =
    [ edge
    | next <- walkingNeighborsRaw world tile
    , Just from <- [nodeFor tile]
    , Just to <- [nodeFor next]
    , edge <- [(from, to, 1, Nothing), (to, from, 1, Nothing)]
    ]

  endpointEdges = concatMap endpointConnections (Set.toList (transportEndpoints world))
  endpointConnections endpoint =
    [ (from, to, 1, Nothing)
    | next <- walkingNeighborsRaw world endpoint
    , Just from <- [nodeFor endpoint]
    , Just to <- [nodeFor next]
    ] <>
    [ (from, to, 1, Just (transportType transport))
    | next <- adjacentTiles endpoint
    , endpoint `elem` walkingNeighborsRaw world next
    , transport <- Map.findWithDefault [] endpoint (worldTransports world)
    , Just from <- [nodeFor next]
    , Just to <- [nodeFor endpoint]
    ]

  localEdges =
    [ (from, to, duration transport, Just (transportType transport))
    | transports <- Map.elems (worldTransports world)
    , transport <- transports
    , transportType transport /= "VIRTUAL_WALL"
    , Just originTile <- [origin transport]
    , Just destinationTile <- [destination transport]
    , Just from <- [nodeFor originTile]
    , Just to <- [nodeFor destinationTile]
    ]

  -- Initial global entry is query-specific; only banking belongs in this static graph.
  globalEntryEdges =
    [ (node, globalId, 0, Nothing)
    | tile <- Set.toList (worldBanks world)
    , Just node <- [nodeFor tile]
    ]
  globalExitEdges =
    [ (globalId, to, duration transport, Just (transportType transport))
    | transport <- worldGlobalTeleports world
    , Just destinationTile <- [destination transport]
    , Just to <- [nodeFor destinationTile]
    ]

  edges = leafEdges <> separatorEdges <> endpointEdges <> localEdges <> globalEntryEdges <> globalExitEdges
  reverseMap = IntMap.fromListWith (<>) [(to, [(from, cost, kind)]) | (from, to, cost, kind) <- edges]
  reversed = Boxed.generate nodeCount (\node -> IntMap.findWithDefault [] node reverseMap)
  nodeFor tile = IntMap.lookup (unTile tile) tileIndex
  transportEndpoints value = Set.fromList
    [ tile
    | transport <- concat (Map.elems (worldTransports value)) <> worldGlobalTeleports value
    , maybeTile <- [origin transport, destination transport]
    , Just tile <- [maybeTile]
    ]
  adjacentTiles tile =
    let (x, y, p) = unpackTile tile
     in [packTile (x + dx) (y + dy) p | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

regionValues :: RegionGraph -> Bool -> Set.Set String -> Tile -> Map.Map Tile Int -> RegionValues
regionValues regionGraph allow enabledTypes target targetDistances =
  RegionValues regionGraph (reverseDijkstra (reverseEdges regionGraph) allow enabledTypes seeds)
 where
  candidates = (target, 0) : Map.toList targetDistances
  seeds = IntMap.toList (IntMap.fromListWith min
    [ (node, distance)
    | (tile, distance) <- candidates
    , Just node <- [IntMap.lookup (unTile tile) (tileNodes regionGraph)]
    ])

reverseDijkstra :: Boxed.Vector [(Int, Int, Maybe String)] -> Bool -> Set.Set String -> [(Int, Int)] -> Vector.Vector Int
reverseDijkstra adjacency allow enabledTypes seeds = runST $ do
  result <- Mutable.replicate (Boxed.length adjacency) maxBound
  queue <- foldM (seed result) PQueue.empty seeds
  let search pending =
        case PQueue.minViewWithKey pending of
          Nothing -> Vector.freeze result
          Just ((cost, node), rest) -> do
            known <- Mutable.read result node
            if cost /= known
              then search rest
              else foldM (relax allow enabledTypes result cost) rest (adjacency Boxed.! node) >>= search
  search queue
 where
  seed result queue (node, cost) = do
    known <- Mutable.read result node
    if cost >= known
      then pure queue
      else Mutable.write result node cost >> pure (PQueue.insert cost node queue)

relax
  :: Bool
  -> Set.Set String
  -> Mutable.MVector s Int
  -> Int
  -> PQueue.MinPQueue Int Int
  -> (Int, Int, Maybe String)
  -> ST s (PQueue.MinPQueue Int Int)
relax allow enabledTypes result cost queue (next, edgeCost, kind) =
  if not (edgeEnabled kind)
    then pure queue
    else case addCost cost edgeCost of
      Nothing -> pure queue
      Just newCost -> do
        known <- Mutable.read result next
        if newCost >= known
          then pure queue
          else Mutable.write result next newCost >> pure (PQueue.insert newCost next queue)
 where
  edgeEnabled Nothing = True
  edgeEnabled (Just edgeType) = allow && (Set.null enabledTypes || Set.member edgeType enabledTypes)

tileLowerBound :: RegionValues -> Tile -> Int
tileLowerBound values tile =
  maybe 0 (finite . (distances values Vector.!))
    (IntMap.lookup (unTile tile) (tileNodes (graph values)))

leafLowerBounds :: RegionValues -> [(LeafId, Int)]
leafLowerBounds values =
  [ (leaf, distance)
  | (leaf, node) <- Map.toList (leafNodes (graph values))
  , let distance = distances values Vector.! node
  , distance /= maxBound
  ]

globalLowerBound :: RegionValues -> Int
globalLowerBound values = finite (distances values Vector.! globalNode (graph values))

finite :: Int -> Int
finite value
  | value == maxBound = 0
  | otherwise = value

minimumMaybe :: [Int] -> Maybe Int
minimumMaybe [] = Nothing
minimumMaybe values = Just (minimum values)

addCost :: Int -> Int -> Maybe Int
addCost a b
  | b < 0 || a > maxBound - b = Nothing
  | otherwise = Just (a + b)
