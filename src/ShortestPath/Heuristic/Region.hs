{-# LANGUAGE MonoLocalBinds #-}

module ShortestPath.Heuristic.Region
  ( RegionGraph
  , RegionValues
  , RegionTable
  , buildRegionGraph
  , buildRegionTable
  , regionGraphEdgeCount
  , regionGraphNodeCount
  , regionValues
  , tileLowerBound
  , tileLowerBounds
  , leafLowerBounds
  , globalLowerBound
  , globalLowerBoundFor
  , tableLowerBound
  , tableGlobalLowerBound
  , tableLeafLowerBounds
  , regionTableSize
  ) where

import Control.Monad (foldM, forM, when)
import Control.Monad.ST (ST, runST)
import Control.Exception (evaluate)
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Min as PQueue
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Word (Word32)
import System.IO (hFlush, stdout)

import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Types
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

data RegionGraph = RegionGraph
  { reverseEdges :: Boxed.Vector [(Int, Int, Maybe String)]
  , reverseStateEdges :: Boxed.Vector [(Int, Int, Maybe String)]
  , tileNodes :: IntMap.IntMap Int
  , leafNodes :: Map.Map LeafId [Int]
  , globalNode :: Int
  , regionGraphEdgeCount :: Int
  }

data RegionValues = RegionValues
  { graph :: RegionGraph
  , distances :: Vector.Vector Int
  }

data RegionTable = RegionTable
  { tableRegions :: Boxed.Vector LeafId
  , tableRegionIndex :: Map.Map LeafId Int
  , tableDistances :: Vector.Vector Word32
  , tableGlobalDistances :: Vector.Vector Word32
  }

instance Binary RegionTable where
  put table = do
    put (Boxed.toList (tableRegions table))
    putVector (tableDistances table)
    putVector (tableGlobalDistances table)
   where
    putVector values = putWord32le (fromIntegral (Vector.length values)) >> Vector.mapM_ putWord32le values
  get = do
    regions <- Boxed.fromList <$> get
    distances <- getVector
    globals <- getVector
    pure (RegionTable regions (Map.fromList (zip (Boxed.toList regions) [0 ..])) distances globals)
   where
    getVector = do
      size <- fromIntegral <$> getWord32le
      Vector.replicateM size getWord32le

regionGraphNodeCount :: RegionGraph -> Int
regionGraphNodeCount = Boxed.length . reverseEdges

buildRegionGraph :: World -> Hierarchy -> RegionGraph
buildRegionGraph world hierarchy =
  RegionGraph reversed stateReversed tileIndex leafIndex globalId (length edges)
 where
  spatialTiles = Set.toAscList (Set.unions
    [ Map.keysSet (terminalLeaf hierarchy)
    , Map.keysSet (hierarchySeparatorNodes hierarchy)
    , transportEndpoints world
    ])
  tileIndex = IntMap.fromList (zip (map unTile spatialTiles) [0 ..])
  tileCount = length spatialTiles
  leafIndex = Map.fromListWith (<>)
    [ (leaf, [node])
    | (tile, leaf) <- Map.toList (terminalLeaf hierarchy)
    , Just node <- [nodeFor tile]
    ]
  globalId = tileCount
  nodeCount = globalId + 1

  leafEdges = concatMap overlayEdges (Map.toList (leafOverlays hierarchy))
  overlayEdges (_, overlay) =
    [ (from, to, distance, Nothing)
    | (tile, adjacent) <- Map.toList (leafTerminalAdjacency overlay)
    , Just from <- [nodeFor tile]
    , (other, distance) <- Map.toList adjacent
    , Just to <- [nodeFor other]
    ]

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
  bankEdges =
    [ (node, node, 0, Nothing, True)
    | tile <- Set.toList (worldBanks world)
    , Just node <- [nodeFor tile]
    ] <>
    [ (node, globalId, 0, Nothing, True)
    | tile <- Set.toList (worldBanks world)
    , Just node <- [nodeFor tile]
    ]
  globalExitEdges =
    [ (globalId, to, duration transport, Just (transportType transport), False)
    | transport <- worldGlobalTeleports world
    , Just destinationTile <- [destination transport]
    , Just to <- [nodeFor destinationTile]
    ]

  ordinaryEdges = leafEdges <> separatorEdges <> endpointEdges <> localEdges
  edges = ordinaryEdges <> [(from, to, cost, kind) | (from, to, cost, kind, _) <- bankEdges <> globalExitEdges]
  reverseMap = IntMap.fromListWith (<>) [(to, [(from, cost, kind)]) | (from, to, cost, kind) <- edges]
  reversed = Boxed.generate nodeCount (\node -> IntMap.findWithDefault [] node reverseMap)
  stateEdges =
    [ (stateId from banked, stateId to banked, cost, kind)
    | (from, to, cost, kind) <- ordinaryEdges
    , banked <- [False, True]
    ] <>
    [ (stateId from False, stateId to True, cost, kind)
    | (from, to, cost, kind, _) <- bankEdges
    ] <>
    [ (stateId from True, stateId to True, cost, kind)
    | (from, to, cost, kind, _) <- globalExitEdges
    ]
  stateReverseMap = IntMap.fromListWith (<>) [(to, [(from, cost, kind)]) | (from, to, cost, kind) <- stateEdges]
  stateReversed = Boxed.generate (nodeCount * 2) (\node -> IntMap.findWithDefault [] node stateReverseMap)
  stateId node banked = node * 2 + if banked then 1 else 0
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

buildRegionTable :: RegionGraph -> IO RegionTable
buildRegionTable regionGraph = do
  let regions = Boxed.fromList (Map.keys (leafNodes regionGraph))
      regionIndex = Map.fromList (zip (Boxed.toList regions) [0 ..])
      count = Boxed.length regions
  rows <- forM [0 .. count - 1] $ \target -> do
    when (target == 0) $ putStrLn ("region lower bounds 0/" <> show count) >> hFlush stdout
    let targetLeaf = regions Boxed.! target
        targetNodes = Map.findWithDefault [] targetLeaf (leafNodes regionGraph)
        seeds = [(node * 2 + state, 0) | node <- targetNodes, state <- [0, 1]]
        values = reverseDijkstra (reverseStateEdges regionGraph) True Set.empty seeds
        regionValue state leaf = minimumFinite
          [ values Vector.! (node * 2 + state)
          | node <- Map.findWithDefault [] leaf (leafNodes regionGraph)
          ]
        row state = Vector.fromList
          [ encodeDistance (regionValue state (regions Boxed.! source))
          | source <- [0 .. count - 1]
          ]
        globals = Vector.fromList
          [ encodeDistance (values Vector.! (globalNode regionGraph * 2 + state))
          | state <- [0, 1]
          ]
        distances = row 0 <> row 1
    _ <- evaluate (Vector.sum distances + Vector.sum globals)
    when ((target + 1) `mod` 25 == 0 || target + 1 == count) $ do
      putStrLn ("region lower bounds " <> show (target + 1) <> "/" <> show count)
      hFlush stdout
    pure (distances, globals)
  pure RegionTable
    { tableRegions = regions
    , tableRegionIndex = regionIndex
    , tableDistances = Vector.concat (map fst rows)
    , tableGlobalDistances = Vector.concat (map snd rows)
    }

tableLowerBound :: RegionTable -> Bool -> [LeafId] -> [LeafId] -> Int
tableLowerBound table banked sources targets = minimumDefault 0
  [ decodeDistance (tableDistances table Vector.! tableOffset table source target banked)
  | sourceLeaf <- sources
  , targetLeaf <- targets
  , Just source <- [Map.lookup sourceLeaf (tableRegionIndex table)]
  , Just target <- [Map.lookup targetLeaf (tableRegionIndex table)]
  ]

tableGlobalLowerBound :: RegionTable -> Bool -> [LeafId] -> Int
tableGlobalLowerBound table banked targets = minimumDefault 0
  [ decodeDistance (tableGlobalDistances table Vector.! (target * 2 + state))
  | targetLeaf <- targets
  , Just target <- [Map.lookup targetLeaf (tableRegionIndex table)]
  ]
 where
  state = if banked then 1 else 0

tableLeafLowerBounds :: RegionTable -> Bool -> [LeafId] -> [(LeafId, Int)]
tableLeafLowerBounds table banked targets =
  [ (leaf, tableLowerBound table banked [leaf] targets)
  | leaf <- Boxed.toList (tableRegions table)
  ]

regionTableSize :: RegionTable -> (Int, Int)
regionTableSize table = (Boxed.length (tableRegions table), Vector.length (tableDistances table))

tableOffset :: RegionTable -> Int -> Int -> Bool -> Int
tableOffset table source target banked = (target * 2 + state) * count + source
 where
  count = Boxed.length (tableRegions table)
  state = if banked then 1 else 0

minimumFinite :: [Int] -> Int
minimumFinite values = minimumDefault maxBound (filter (/= maxBound) values)

minimumDefault :: Ord a => a -> [a] -> a
minimumDefault fallback [] = fallback
minimumDefault _ values = minimum values

encodeDistance :: Int -> Word32
encodeDistance value
  | value == maxBound = maxBound
  | otherwise = fromIntegral value

decodeDistance :: Word32 -> Int
decodeDistance value
  | value == maxBound = 0
  | otherwise = fromIntegral value

regionValues :: RegionGraph -> Bool -> Set.Set String -> Tile -> Map.Map Tile Int -> RegionValues
regionValues regionGraph allow enabledTypes target targetDistances =
  RegionValues regionGraph (reverseDijkstra (reverseStateEdges regionGraph) allow enabledTypes seeds)
 where
  candidates = (target, 0) : Map.toList targetDistances
  seeds = IntMap.toList (IntMap.fromListWith min
    [ (node * 2 + state, distance)
    | (tile, distance) <- candidates
    , Just node <- [IntMap.lookup (unTile tile) (tileNodes regionGraph)]
    , state <- [0, 1]
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

tileLowerBound :: RegionValues -> Bool -> Tile -> Int
tileLowerBound values banked tile =
  maybe 0 (finite . (distances values Vector.!) . stateId)
    (IntMap.lookup (unTile tile) (tileNodes (graph values)))
 where
  stateId node = node * 2 + if banked then 1 else 0

tileLowerBounds :: RegionValues -> [(Tile, Int)]
tileLowerBounds values =
  [ (Tile packed, distance)
  | (packed, node) <- IntMap.toList (tileNodes (graph values))
  , let distance = distances values Vector.! (node * 2)
  , distance /= maxBound
  ]

leafLowerBounds :: RegionValues -> [(LeafId, Int)]
leafLowerBounds values =
  [ (leaf, minimum reachable)
  | (leaf, nodes) <- Map.toList (leafNodes (graph values))
  , let reachable = [distance | node <- nodes, let distance = distances values Vector.! (node * 2), distance /= maxBound]
  , not (null reachable)
  ]

globalLowerBound :: RegionValues -> Int
globalLowerBound values = globalLowerBoundFor values False

globalLowerBoundFor :: RegionValues -> Bool -> Int
globalLowerBoundFor values banked = finite (distances values Vector.! (globalNode (graph values) * 2 + state))
 where
  state = if banked then 1 else 0

finite :: Int -> Int
finite value
  | value == maxBound = 0
  | otherwise = value

addCost :: Int -> Int -> Maybe Int
addCost a b
  | b < 0 || a > maxBound - b = Nothing
  | otherwise = Just (a + b)
