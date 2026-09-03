{-# LANGUAGE MonoLocalBinds #-}

module ShortestPath.Exact.TileAStar
  ( TileAStar(..)
  , Box(..)
  , NaturalComponents(..)
  , TileStatic(..)
  , TileAStarCounters(..)
  , TileReverseCounters(..)
  , TileAStarTimings(..)
  , SparseWalkingNetwork(..)
  , buildTileAStar
  , buildSparseWalkingNetwork
  , sparseWalkingDistance
  , chebyshevTransform
  , chebyshevTransformC
  , chebyshevTransformSlow
  , componentBox
  , componentTileGroups
  , findRouteProfiledTileAStar
  , forceTileAStar
  , renderHeuristicTiles
  , tileStaticStats
  , HeuristicRender(..)
  , HeuristicLayer(..)
  , HeuristicTile(..)
  ) where

import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Binary (Binary(..))
import Data.List (sortBy)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Ord (comparing)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (mallocForeignPtrArray, withForeignPtr)
import GHC.Clock (getMonotonicTimeNSec)
import Foreign.Ptr (Ptr)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

foreign import ccall unsafe "spm_chebyshev_transform"
  c_chebyshevTransform :: Int -> Int -> Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

foreign import ccall unsafe "spm_rgba_tile"
  c_rgbaTile :: Int -> Ptr Int -> Ptr Int -> Ptr Int -> Int -> Int -> Int -> Int -> Int -> Ptr Word8 -> IO ()

data TileAStar = TileAStar World NaturalComponents TileStatic

data NaturalComponents = NaturalComponents
  { componentOwnerTiles :: Vector.Vector Int
  , componentOwnerIds :: Vector.Vector Int
  , componentIds :: Vector.Vector Int
  , maxComponentId :: !Int
  }

instance Binary NaturalComponents where
  put components = do
    put (Vector.toList (componentOwnerTiles components))
    put (Vector.toList (componentOwnerIds components))
    put (Vector.toList (componentIds components))
    put (maxComponentId components)
  get =
    NaturalComponents
      <$> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> get

data TileStatic = TileStatic
  { staticTiles :: Vector.Vector Int
  , staticComponents :: Vector.Vector Int
  , staticWalkingNetwork :: SparseWalkingNetwork
  }

instance Binary TileStatic where
  put value = do
    put (Vector.toList (staticTiles value))
    put (Vector.toList (staticComponents value))
    put (staticWalkingNetwork value)
  get =
    TileStatic
      <$> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> get

data SparseWalkingNetwork = SparseWalkingNetwork
  { sparseOriginalCount :: !Int
  , sparseVertexCount :: !Int
  , sparseSteinerCount :: !Int
  , sparseWalkingEdgeCount :: !Int
  , sparseOffsets :: Vector.Vector Int
  , sparseDestinations :: Vector.Vector Int
  , sparseWeights :: Vector.Vector Int
  }
  deriving stock (Eq, Show)

instance Binary SparseWalkingNetwork where
  put value = do
    put (sparseOriginalCount value)
    put (sparseVertexCount value)
    put (sparseSteinerCount value)
    put (sparseWalkingEdgeCount value)
    put (Vector.toList (sparseOffsets value))
    put (Vector.toList (sparseDestinations value))
    put (Vector.toList (sparseWeights value))
  get =
    SparseWalkingNetwork
      <$> get
      <*> get
      <*> get
      <*> get
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)
      <*> (Vector.fromList <$> get)

data Box = Box
  { boxMinX :: !Int
  , boxMinY :: !Int
  , boxMaxX :: !Int
  , boxMaxY :: !Int
  , boxPlane :: !Int
  }
  deriving stock (Eq, Show)

data State = State Tile Bool
  deriving stock (Eq, Ord, Show)

data SearchSpace = SearchSpace
  { searchBaseTiles :: Vector.Vector Int
  , searchExtraTiles :: Vector.Vector Int
  , searchSize :: !Int
  }

data MutableQueue s = MutableQueue
  { queuePriorities :: Mutable.MVector s Int
  , queueStates :: Mutable.MVector s Int
  , queueCosts :: Mutable.MVector s Int
  , queueSizeRef :: STRef s Int
  }

data HeuristicRender = HeuristicRender
  { renderTileSize :: !Int
  , renderLayers :: [HeuristicLayer]
  }
  deriving stock (Eq, Show)

data HeuristicLayer = HeuristicLayer
  { layerKey :: String
  , layerLabel :: String
  , layerBankPathEnabled :: !Bool
  , layerMinimum :: !Int
  , layerMaximum :: !Int
  , layerHeuristicMilliseconds :: !Double
  , layerTransformMilliseconds :: !Double
  , layerWriteMilliseconds :: !Double
  , layerTiles :: [HeuristicTile]
  }
  deriving stock (Eq, Show)

data HeuristicTile = HeuristicTile
  { tileUrl :: String
  , tilePlane :: !Int
  , tileX :: !Int
  , tileY :: !Int
  , tileMinimum :: !Int
  , tileMaximum :: !Int
  }
  deriving stock (Eq, Show)

tileStaticStats :: TileAStar -> (Int, Int, Int, Int)
tileStaticStats (TileAStar _ _ static) =
  ( Vector.length (staticTiles static)
  , sparseSteinerCount network
  , sparseVertexCount network
  , sparseWalkingEdgeCount network
  )
 where
  network = staticWalkingNetwork static

data TileAStarCounters = TileAStarCounters
  { tileStatesPopped :: !Int
  , tileStalePqEntries :: !Int
  , tilePqPushes :: !Int
  , tileUniqueStatesReached :: !Int
  , tileWalkingRelaxations :: !Int
  , tileTransportRelaxations :: !Int
  , tileHeuristicEvaluations :: !Int
  }
  deriving stock (Eq, Show)

data TileReverseCounters = TileReverseCounters
  { reverseStatesPopped :: !Int
  , reverseStalePqEntries :: !Int
  , reversePqPushes :: !Int
  , reversePqPops :: !Int
  , reverseSameComponentSiteScans :: !Int
  , reverseTotalSitesScanned :: !Int
  , reverseChebyshevComparisons :: !Int
  , reverseMaxSitesScannedPerPop :: !Int
  , reverseTransportRelaxations :: !Int
  }
  deriving stock (Eq, Show)

data TileAStarTimings = TileAStarTimings
  { tileHeuristicSetupMilliseconds :: !Double
  , tileReverseDijkstraMilliseconds :: !Double
  , tileSeedTableMilliseconds :: !Double
  , tileSearchMilliseconds :: !Double
  , tileTotalMilliseconds :: !Double
  , tileSearchCounters :: !TileAStarCounters
  , tileReverseCounters :: !TileReverseCounters
  }
  deriving stock (Eq, Show)

data Heuristic = Heuristic
  { heuristicSeeds :: Boxed.Vector (Vector.Vector (Int, Int))
  , heuristicReverseMilliseconds :: !Double
  , heuristicSeedTableMilliseconds :: !Double
  , heuristicReverseCounters :: !TileReverseCounters
  }

data SiteGraph = SiteGraph
  { siteTiles :: Vector.Vector Int
  , siteTileIndex :: IntMap.IntMap Int
  , siteComponents :: Vector.Vector Int
  , siteStaticCount :: !Int
  , siteSparseNetwork :: SparseWalkingNetwork
  , siteComponentSiteIds :: Boxed.Vector (Vector.Vector Int)
  , siteReverseEdges :: Boxed.Vector (Vector.Vector (Int, Int))
  }

instance RouteFinder TileAStar where
  routeName _ = "tile-astar"
  findRoute astar q = unsafePerformIO (fst <$> findRouteProfiledTileAStar astar q)

buildTileAStar :: World -> IO TileAStar
buildTileAStar world = do
  components <- naturalComponents world
  let static = buildTileStatic world components
  pure (TileAStar world components static)

forceTileAStar :: TileAStar -> IO TileAStar
forceTileAStar astar@(TileAStar _ components static) = do
  _ <- evaluate
    ( Vector.length (componentOwnerTiles components)
        + Vector.length (componentOwnerIds components)
        + Vector.length (componentIds components)
        + maxComponentId components
        + Vector.length (staticTiles static)
        + sparseVertexCount (staticWalkingNetwork static)
        + sparseWalkingEdgeCount (staticWalkingNetwork static)
    )
  pure astar

findRouteProfiledTileAStar :: TileAStar -> Query -> IO (Route, TileAStarTimings)
findRouteProfiledTileAStar astar@(TileAStar _ _ _) query =
  do
    (heuristic, setupMs) <- timedIO forceHeuristic (buildHeuristic astar query)
    ((route, counters), searchMs) <- timedIO forceSearch (pure (search astar query heuristic))
    let totalMs = setupMs + searchMs
    pure
      ( route
      , TileAStarTimings
          setupMs
          (heuristicReverseMilliseconds heuristic)
          (heuristicSeedTableMilliseconds heuristic)
          searchMs
          totalMs
          counters
          (heuristicReverseCounters heuristic)
      )

search :: TileAStar -> Query -> Heuristic -> (Route, TileAStarCounters)
search astar@(TileAStar world components _) q heuristic = runST $ do
  best <- Mutable.replicate stateCount maxBound
  prevState <- Mutable.replicate stateCount maxBound
  prevStep <- BoxedMutable.replicate stateCount Nothing
  -- ponytail: fixed initial heap cap; switch to a growable heap when benchmark routes exceed it.
  queue <- queueNew (min stateCount 262144)
  Mutable.write best startState 0
  queuePush queue (evalH (queryStart q) False) startState 0
  go best prevState prevStep queue emptyCounters {tilePqPushes = 1, tileUniqueStatesReached = 1, tileHeuristicEvaluations = 1}
 where
  space = searchSpace astar q
  stateCount = searchSize space * 2
  startNode = maybe 0 id (nodeFor (queryStart q))
  startState = stateId startNode False
  target = queryTarget q

  go ::
    Mutable.MVector s Int ->
    Mutable.MVector s Int ->
    BoxedMutable.MVector s (Maybe RouteStep) ->
    MutableQueue s ->
    TileAStarCounters ->
    ST s (Route, TileAStarCounters)
  go best prevState prevStep queue counters = do
    popped <- queuePop queue
    case popped of
      Nothing -> pure (Route maxBound (tileStatesPopped counters) [], counters)
      Just (_, state, cost) -> do
        known <- Mutable.read best state
        if cost /= known
          then go best prevState prevStep queue counters {tileStalePqEntries = tileStalePqEntries counters + 1}
          else do
            let tile = stateTile state
            if tile == target
              then do
                steps <- reconstruct prevState prevStep state
                pure (Route cost (tileStatesPopped counters) steps, counters)
              else do
                let counters' = counters {tileStatesPopped = tileStatesPopped counters + 1}
                counters'' <- foldM (relax best prevState prevStep queue cost state) counters' (neighbors state)
                go best prevState prevStep queue counters''

  relax ::
    Mutable.MVector s Int ->
    Mutable.MVector s Int ->
    BoxedMutable.MVector s (Maybe RouteStep) ->
    MutableQueue s ->
    Int ->
    Int ->
    TileAStarCounters ->
    (Int, Int, RouteStep, EdgeKind) ->
    ST s TileAStarCounters
  relax best prevState prevStep queue cost state counters (next, stepCost, step, kind) =
    case addCost cost stepCost of
      Nothing -> pure counters
      Just newCost -> do
        known <- Mutable.read best next
        if newCost >= known
          then pure (countKind kind counters)
          else do
            Mutable.write best next newCost
            Mutable.write prevState next state
            BoxedMutable.write prevStep next (Just step)
            let priority = addCostDefault maxBound newCost (evalH (stateTile next) (stateBanked next))
                counted = countKind kind counters
                counters' = counted
                  { tilePqPushes = tilePqPushes counted + 1
                  , tileUniqueStatesReached = tileUniqueStatesReached counted + if known == maxBound then 1 else 0
                  , tileHeuristicEvaluations = tileHeuristicEvaluations counted + 1
                  }
            queuePush queue priority next newCost
            pure counters'

  countKind WalkingEdge counters = counters {tileWalkingRelaxations = tileWalkingRelaxations counters + 1}
  countKind TransportEdge counters = counters {tileTransportRelaxations = tileTransportRelaxations counters + 1}

  evalH tile banked = heuristicAt components heuristic (State tile banked)

  neighbors state =
    walk <> bank <> localTransports <> initialGlobalTransports <> bankGlobalTransports
   where
    tile = stateTile state
    banked = stateBanked state
    walk =
      [ (next, 1, Walk t, WalkingEdge)
      | t <- walkingNeighborsRaw world tile
      , isWalkable (worldCollision world) t || usableOrigin banked t
      , Just next <- [stateFor t banked]
      ]
    bank =
      [ (next, 0, Walk tile, TransportEdge)
      | bankPathEnabled q
      , not banked
      , Set.member tile (worldBanks world)
      , Just next <- [stateFor tile True]
      ]
    localTransports =
      if allowTransports q
        then transportEdges banked (filter ((/= "VIRTUAL_WALL") . transportType) (Map.findWithDefault [] tile (worldTransports world)))
        else []
    initialGlobalTransports =
      [ edge
      | allowTransports q
      , not banked
      , tile == queryStart q
      , edge <- transportEdges False (worldGlobalTeleports world)
      ]
    bankGlobalTransports =
      [ (next, stepCost, step, TransportEdge)
      | allowTransports q
      , bankPathEnabled q
      , not banked
      , Set.member tile (worldBanks world)
      , (next, stepCost, step, _) <- transportEdges True (worldGlobalTeleports world)
      ]

  transportEdges banked transports =
    [ (next, transportCost t, UseTransport (label t) dst, TransportEdge)
    | t <- transports
    , transportAvailable q banked t
    , Just dst <- [destination t]
    , Just next <- [stateFor dst banked]
    ]

  usableOrigin banked tile = allowTransports q && any (transportAvailable q banked) (Map.findWithDefault [] tile (worldTransports world))
  transportCost t = duration t + Map.findWithDefault 0 (transportType t) (transportPenalties q)
  label t = if null (displayInfo t) then transportType t else displayInfo t

  stateFor tile banked = flip stateId banked <$> nodeFor tile
  nodeFor tile = searchNodeFor space tile
  stateTile state = Tile (searchTileAt space (state `div` 2))
  stateBanked state = odd state

  reconstruct ::
    Mutable.MVector s Int ->
    BoxedMutable.MVector s (Maybe RouteStep) ->
    Int ->
    ST s [RouteStep]
  reconstruct prevState prevStep state = reverse <$> collect state
   where
    collect s = do
      p <- Mutable.read prevState s
      step <- BoxedMutable.read prevStep s
      case step of
        Nothing -> pure []
        Just value -> (value :) <$> collect p

data EdgeKind = WalkingEdge | TransportEdge

emptyCounters :: TileAStarCounters
emptyCounters = TileAStarCounters 0 0 0 0 0 0 0

emptyReverseCounters :: TileReverseCounters
emptyReverseCounters = TileReverseCounters 0 0 0 0 0 0 0 0 0

searchSpace :: TileAStar -> Query -> SearchSpace
searchSpace (TileAStar world components _) q =
  SearchSpace base extras (Vector.length base + Vector.length extras)
 where
  base = componentOwnerTiles components
  endpoints =
    queryStart q : queryTarget q : Set.toList (worldBanks world) <>
      [ tile
      | t <- concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
      , Just tile <- [origin t] <> [destination t]
      ]
  extras =
    Vector.fromList
      [ packed
      | packed <- IntSet.toAscList (IntSet.fromList (map unTile endpoints))
      , binarySearch packed base == Nothing
      ]

searchNodeFor :: SearchSpace -> Tile -> Maybe Int
searchNodeFor space tile =
  case binarySearch packed (searchBaseTiles space) of
    Just ix -> Just ix
    Nothing -> (+ Vector.length (searchBaseTiles space)) <$> binarySearch packed (searchExtraTiles space)
 where
  packed = unTile tile

searchTileAt :: SearchSpace -> Int -> Int
searchTileAt space node
  | node < baseLength = searchBaseTiles space Vector.! node
  | otherwise = searchExtraTiles space Vector.! (node - baseLength)
 where
  baseLength = Vector.length (searchBaseTiles space)

queueNew :: Int -> ST s (MutableQueue s)
queueNew capacity = do
  priorities <- Mutable.new (max 1 capacity)
  states <- Mutable.new (max 1 capacity)
  costs <- Mutable.new (max 1 capacity)
  size <- newSTRef 0
  pure (MutableQueue priorities states costs size)

queuePush :: MutableQueue s -> Int -> Int -> Int -> ST s ()
queuePush queue priority state cost = do
  size <- readSTRef (queueSizeRef queue)
  when (size >= Mutable.length (queuePriorities queue)) (error "tile astar priority queue capacity exceeded")
  Mutable.write (queuePriorities queue) size priority
  Mutable.write (queueStates queue) size state
  Mutable.write (queueCosts queue) size cost
  writeSTRef (queueSizeRef queue) (size + 1)
  queueBubbleUp queue size

queuePop :: MutableQueue s -> ST s (Maybe (Int, Int, Int))
queuePop queue = do
  size <- readSTRef (queueSizeRef queue)
  if size == 0
    then pure Nothing
    else do
      priority <- Mutable.read (queuePriorities queue) 0
      state <- Mutable.read (queueStates queue) 0
      cost <- Mutable.read (queueCosts queue) 0
      let lastIx = size - 1
      writeSTRef (queueSizeRef queue) lastIx
      when (lastIx > 0) $ do
        moveQueueEntry queue lastIx 0
        queueBubbleDown queue 0
      pure (Just (priority, state, cost))

queueBubbleUp :: MutableQueue s -> Int -> ST s ()
queueBubbleUp queue ix
  | ix <= 0 = pure ()
  | otherwise = do
      let parent = (ix - 1) `div` 2
      childEntry <- queueEntry queue ix
      parentEntry <- queueEntry queue parent
      if queueEntryLess childEntry parentEntry
        then swapQueueEntries queue ix parent >> queueBubbleUp queue parent
        else pure ()

queueBubbleDown :: MutableQueue s -> Int -> ST s ()
queueBubbleDown queue ix = do
  size <- readSTRef (queueSizeRef queue)
  let left = ix * 2 + 1
      right = left + 1
  if left >= size
    then pure ()
    else do
      smallest <- do
        leftEntry <- queueEntry queue left
        if right >= size
          then pure left
          else do
            rightEntry <- queueEntry queue right
            pure (if queueEntryLess rightEntry leftEntry then right else left)
      here <- queueEntry queue ix
      child <- queueEntry queue smallest
      if queueEntryLess child here
        then swapQueueEntries queue ix smallest >> queueBubbleDown queue smallest
        else pure ()

queueEntry :: MutableQueue s -> Int -> ST s (Int, Int, Int)
queueEntry queue ix = do
  (,,) <$> Mutable.read (queuePriorities queue) ix <*> Mutable.read (queueStates queue) ix <*> Mutable.read (queueCosts queue) ix

queueEntryLess :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
queueEntryLess (leftPriority, leftState, leftCost) (rightPriority, rightState, rightCost) =
  (leftPriority, leftCost, leftState) < (rightPriority, rightCost, rightState)

swapQueueEntries :: MutableQueue s -> Int -> Int -> ST s ()
swapQueueEntries queue left right = do
  entry <- queueEntry queue left
  moveQueueEntry queue right left
  writeQueueEntry queue right entry

moveQueueEntry :: MutableQueue s -> Int -> Int -> ST s ()
moveQueueEntry queue from to = queueEntry queue from >>= writeQueueEntry queue to

writeQueueEntry :: MutableQueue s -> Int -> (Int, Int, Int) -> ST s ()
writeQueueEntry queue ix (priority, state, cost) = do
  Mutable.write (queuePriorities queue) ix priority
  Mutable.write (queueStates queue) ix state
  Mutable.write (queueCosts queue) ix cost

buildHeuristic :: TileAStar -> Query -> IO Heuristic
buildHeuristic astar@(TileAStar _ components _) q =
  transportAware
 where
  target = queryTarget q
  transportAware = do
    let graph = siteGraph astar q
    countReverse <- (== Just "1") <$> lookupEnv "SPM_TILE_REVERSE_COUNTERS"
    useManhattan <- (== Just "manhattan") <$> lookupEnv "SPM_TILE_REVERSE_IMPL"
    compareReverse <- (== Just "1") <$> lookupEnv "SPM_TILE_COMPARE_REVERSE"
    ((distances, counters), reverseMs) <-
      if useManhattan
        then timedIO forceReverseResult $ do
          let manhattan = halveDistances (reverseDijkstraManhattanUncounted graph (targetSeedsManhattan graph target))
          when compareReverse (assertReverseLabelsEqual graph (reverseDijkstraUncounted graph (targetSeeds graph target)) manhattan)
          pure (manhattan, emptyReverseCounters)
        else if countReverse
        then timedIO forceReverseResult (pure (reverseDijkstra graph (targetSeeds graph target)))
        else timedIO forceReverseResult (pure (reverseDijkstraUncounted graph (targetSeeds graph target), emptyReverseCounters))
    (table, seedMs) <- timedIO forceSeedTable (pure (seedTableFromDistances components graph distances))
    pure (Heuristic table reverseMs seedMs counters)

renderHeuristicTiles :: TileAStar -> Query -> FilePath -> String -> IO HeuristicRender
renderHeuristicTiles astar@(TileAStar _ components _) q outputRoot urlRoot = do
  createDirectoryIfMissing True outputRoot
  useCTransform <- (== Just "c") <$> lookupEnv "SPM_HEURISTIC_TRANSFORM"
  layers <- mapM (renderLayer useCTransform) [("no-bank", "Banking disabled", False), ("bank", "Banking enabled", True)]
  pure (HeuristicRender imageTileSize layers)
 where
  groups = componentTileGroups components
  transform useCTransform box seeds =
    (if useCTransform then chebyshevTransformC else chebyshevTransform) box seeds
  renderLayer useCTransform (key, title, banking) = do
    let q' = q {bankPathEnabled = banking}
    (heuristic, heuristicMs) <- timedIO forceHeuristic (buildHeuristic astar q')
    (points, transformMs) <- timedIO forcePointList (pure (layerPoints (transform useCTransform) groups heuristic False))
    let
        values = map snd points
        minimumValue = minimumDefault 0 values
        maximumValue = maximumDefault 0 values
        layerDir = outputRoot </> key
        layerUrl = urlRoot <> "/" <> key
    createDirectoryIfMissing True layerDir
    (tiles, writeMs) <- timedIO (evaluate . length) (writeHeuristicImageTiles key minimumValue maximumValue layerDir layerUrl points)
    pure (HeuristicLayer key title banking minimumValue maximumValue heuristicMs transformMs writeMs tiles)

componentTileGroups :: NaturalComponents -> Boxed.Vector (Vector.Vector Int)
componentTileGroups components = runST $ do
  counts <- Mutable.replicate groupCount 0
  Vector.forM_ (componentOwnerIds components) $ \cid ->
    Mutable.modify counts (+ 1) cid
  frozenCounts <- Vector.freeze counts
  let starts = Vector.scanl' (+) 0 frozenCounts
  cursors <- Vector.thaw starts
  grouped <- Mutable.new (Vector.length (componentOwnerTiles components))
  Vector.iforM_ (componentOwnerTiles components) $ \ix packed -> do
    let cid = componentOwnerIds components Vector.! ix
    writeIx <- Mutable.read cursors cid
    Mutable.write grouped writeIx packed
    Mutable.write cursors cid (writeIx + 1)
  frozenGrouped <- Vector.freeze grouped
  pure (Boxed.generate groupCount (\cid -> Vector.slice (starts Vector.! cid) (frozenCounts Vector.! cid) frozenGrouped))
 where
  groupCount = maxComponentId components + 1

buildTileStatic :: World -> NaturalComponents -> TileStatic
buildTileStatic world components =
  TileStatic tiles comps network
 where
  sites = Set.toAscList (Set.fromList (staticEndpoints <> Set.toList (worldBanks world)))
  staticEndpoints =
    [ tile
    | t <- concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
    , Just tile <- [origin t] <> [destination t]
    ]
  tiles = Vector.fromList (map unTile sites)
  comps = Vector.map (\packed -> maybe (-1) id (componentOf components (Tile packed))) tiles
  network = buildSparseWalkingNetworkComponents (Vector.length tiles) [(comps Vector.! ix, ix, Tile packed) | (ix, packed) <- Vector.toList (Vector.indexed tiles), comps Vector.! ix >= 0]

data ManhattanPoint = ManhattanPoint
  { pointOriginal :: !Int
  , pointA :: !Int
  , pointB :: !Int
  }
  deriving stock (Eq, Show)

buildSparseWalkingNetwork :: [(Int, Tile)] -> SparseWalkingNetwork
buildSparseWalkingNetwork sites =
  SparseWalkingNetwork originalCount vertexCount steinerCount edgeCount offsets destinations weights
 where
  points = map toPoint sites
  originalCount = if null sites then 0 else maximum (map fst sites) + 1
  (nextVertex, edges) = buildManhattanEdges originalCount points
  vertexCount = nextVertex
  steinerCount = vertexCount - originalCount
  edgeCount = length edges
  (offsets, destinations, weights) = undirectedAdjacency vertexCount edges
  toPoint (siteId, tile) =
    let (x, y, _) = unpackTile tile
     in ManhattanPoint siteId (x + y) (x - y)

sparseWalkingDistance :: SparseWalkingNetwork -> Int -> Int -> Maybe Int
sparseWalkingDistance network source target
  | source < 0 || source >= sparseOriginalCount network = Nothing
  | target < 0 || target >= sparseOriginalCount network = Nothing
  | otherwise = finiteDistance (dijkstra source Vector.! target)
 where
  finiteDistance value
    | value == maxBound = Nothing
    | otherwise = Just value
  dijkstra start = runST $ do
    result <- Mutable.replicate (sparseVertexCount network) maxBound
    queue <- queueNew (max 1 (sparseWalkingEdgeCount network * 4 + 1))
    Mutable.write result start 0
    queuePush queue 0 start 0
    let go = do
          popped <- queuePop queue
          case popped of
            Nothing -> Vector.freeze result
            Just (_, node, cost) -> do
              known <- Mutable.read result node
              if cost /= known
                then go
                else do
                  relaxEdges result queue cost node
                  go
    go
  relaxEdges :: Mutable.MVector s Int -> MutableQueue s -> Int -> Int -> ST s ()
  relaxEdges result queue cost node =
    goEdges (sparseOffsets network Vector.! node)
   where
    end = sparseOffsets network Vector.! (node + 1)
    goEdges ix
      | ix >= end = pure ()
      | otherwise = do
          let next = sparseDestinations network Vector.! ix
              edgeCost = sparseWeights network Vector.! ix
          case addCost cost edgeCost of
            Nothing -> pure ()
            Just newCost -> do
              old <- Mutable.read result next
              when (newCost < old) $ do
                Mutable.write result next newCost
                queuePush queue newCost next newCost
          goEdges (ix + 1)

buildSparseWalkingNetworkComponents :: Int -> [(Int, Int, Tile)] -> SparseWalkingNetwork
buildSparseWalkingNetworkComponents originalCount sites =
  SparseWalkingNetwork originalCount vertexCount steinerCount edgeCount offsets destinations weights
 where
  grouped = Map.elems (Map.fromListWith (<>) [(cid, [(siteId, tile)]) | (cid, siteId, tile) <- sites])
  (vertexCount, edges) = foldl' addComponent (originalCount, []) grouped
  steinerCount = vertexCount - originalCount
  edgeCount = length edges
  (offsets, destinations, weights) = undirectedAdjacency vertexCount edges
  addComponent (next, allEdges) componentSites =
    let points = map toPoint componentSites
        (next', componentEdges) = buildManhattanEdges next points
     in (next', componentEdges <> allEdges)
  toPoint (siteId, tile) =
    let (x, y, _) = unpackTile tile
     in ManhattanPoint siteId (x + y) (x - y)

data SplitAxis = SplitA | SplitB

buildManhattanEdges :: Int -> [ManhattanPoint] -> (Int, [(Int, Int, Int)])
buildManhattanEdges firstSteiner points = go firstSteiner (sortPoints points) []
 where
  sortPoints = sortBy (comparing pointA <> comparing pointB <> comparing pointOriginal)
  go next [] edges = (next, edges)
  go next [_] edges = (next, edges)
  go next pts@(firstPoint:_:_) edges
    | allSameA && allSameB = (next, zeroChain pts edges)
    | allSameA = splitOn SplitB next pts edges
    | otherwise = splitOn SplitA next pts edges
   where
    allSameA = all ((== pointA firstPoint) . pointA) pts
    allSameB = all ((== pointB firstPoint) . pointB) pts
  splitOn axis next pts edges =
    let sorted = sortBy (comparing (coordOf axis) <> comparing pointA <> comparing pointB <> comparing pointOriginal) pts
        (left, right, m) = balancedGapSplit (coordOf axis) sorted
        (next', edges') = addProjectionLevel axis m next pts edges
        (next'', edges'') = go next' left edges'
     in go next'' right edges''
  zeroChain [] edges = edges
  zeroChain [_] edges = edges
  zeroChain (p:q:rest) edges = zeroChain (q:rest) ((pointOriginal p, pointOriginal q, 0) : edges)

balancedGapSplit :: (ManhattanPoint -> Int) -> [ManhattanPoint] -> ([ManhattanPoint], [ManhattanPoint], Int)
balancedGapSplit coord pts =
  pickGap preferred gaps
 where
  n = length pts
  preferred = n `div` 2
  indexed = zip [1 :: Int ..] (zip pts (drop 1 pts))
  gaps = [(abs (i - preferred), i, coord left, coord right) | (i, (left, right)) <- indexed, coord left < coord right]
  pickGap _ [] = error "sparse walking split without a strict coordinate gap"
  pickGap _ candidates =
    let (_, i, l, r) = minimum candidates
     in (take i pts, drop i pts, (l + r) `div` 2)

coordOf :: SplitAxis -> ManhattanPoint -> Int
coordOf SplitA = pointA
coordOf SplitB = pointB

otherCoordOf :: SplitAxis -> ManhattanPoint -> Int
otherCoordOf SplitA = pointB
otherCoordOf SplitB = pointA

addProjectionLevel :: SplitAxis -> Int -> Int -> [ManhattanPoint] -> [(Int, Int, Int)] -> (Int, [(Int, Int, Int)])
addProjectionLevel axis m firstProjection pts edges =
  (firstProjection + length projections, chain sortedProjections (spokes <> edges))
 where
  projections = zip [firstProjection ..] pts
  spokes = [(pointOriginal p, steiner, abs (coordOf axis p - m)) | (steiner, p) <- projections]
  chain sorted edgesSoFar =
    foldr addChain edgesSoFar (zip sorted (drop 1 sorted))
  sortedProjections =
    sortBy (comparing (otherCoordOf axis . snd) <> comparing (pointOriginal . snd)) projections
  addChain ((leftId, left), (rightId, right)) edgesSoFar =
    (leftId, rightId, abs (otherCoordOf axis left - otherCoordOf axis right)) : edgesSoFar

undirectedAdjacency :: Int -> [(Int, Int, Int)] -> (Vector.Vector Int, Vector.Vector Int, Vector.Vector Int)
undirectedAdjacency size edges = runST $ do
  counts <- Mutable.replicate size 0
  forM_ edges $ \(a, b, _) -> do
    Mutable.modify counts (+ 1) a
    Mutable.modify counts (+ 1) b
  frozenCounts <- Vector.freeze counts
  let offsets = Vector.scanl' (+) 0 frozenCounts
  cursors <- Vector.thaw offsets
  destinations <- Mutable.new (Vector.last offsets)
  weights <- Mutable.new (Vector.last offsets)
  forM_ edges $ \(a, b, cost) -> do
    add cursors destinations weights a b cost
    add cursors destinations weights b a cost
  frozenDestinations <- Vector.freeze destinations
  frozenWeights <- Vector.freeze weights
  pure (offsets, frozenDestinations, frozenWeights)
 where
  add cursors destinations weights from to cost = do
    ix <- Mutable.read cursors from
    Mutable.write destinations ix to
    Mutable.write weights ix cost
    Mutable.write cursors from (ix + 1)

layerPoints :: (Box -> [(Tile, Int)] -> Vector.Vector Int) -> Boxed.Vector (Vector.Vector Int) -> Heuristic -> Bool -> [(Int, Int)]
layerPoints transform groups heuristic banked =
  concatMap componentPoints (filter (not . Vector.null . snd) (Boxed.toList (Boxed.indexed groups)))
 where
  componentPoints (cid, packedTiles) =
    let seeds = heuristicSeeds heuristic Boxed.! seedKey cid banked
     in if Vector.null seeds
          then []
          else
            let box = componentBox packedTiles
                field = transform box [(Tile packed, cost) | (packed, cost) <- Vector.toList seeds]
             in [ (packed, value)
                | packed <- Vector.toList packedTiles
                , Just ix <- [offset box (Tile packed)]
                , let value = field Vector.! ix
                , value /= maxBound
                ]

componentBox :: Vector.Vector Int -> Box
componentBox packedTiles
  | Vector.null packedTiles = Box 0 0 0 0 0
  | otherwise = Vector.foldl' expand (Box x y x y p) (Vector.tail packedTiles)
 where
  packed = Vector.head packedTiles
  (x, y, p) = unpackTile (Tile packed)
  expand box tilePacked =
    let (tx, ty, _) = unpackTile (Tile tilePacked)
     in box
          { boxMinX = min (boxMinX box) tx
          , boxMinY = min (boxMinY box) ty
          , boxMaxX = max (boxMaxX box) tx
          , boxMaxY = max (boxMaxY box) ty
          }

writeHeuristicImageTiles :: String -> Int -> Int -> FilePath -> String -> [(Int, Int)] -> IO [HeuristicTile]
writeHeuristicImageTiles key minimumValue maximumValue layerDir layerUrl points = do
  let grouped = IntMap.toList (foldl addPoint IntMap.empty points)
  mapM writeTile grouped
 where
  addPoint tiles (packed, value) =
    let (x, y, plane) = unpackTile (Tile packed)
        tx = x `div` imageTileSize
        ty = y `div` imageTileSize
     in IntMap.insertWith (++) (imageTileKey plane tx ty) [(x, y, value)] tiles

  writeTile (encoded, tilePoints) = do
    let (plane, tx, ty) = decodeImageTileKey encoded
        values = map (\(_, _, value) -> value) tilePoints
        tileMin = minimumDefault 0 values
        tileMax = maximumDefault 0 values
        fileName = printf "%d_%d_%d.rgba" plane tx ty
        filePath = layerDir </> fileName
        url = layerUrl <> "/" <> fileName
    pixels <- imagePixels key minimumValue maximumValue tx ty tilePoints
    BS.writeFile filePath pixels
    pure (HeuristicTile url plane tx ty tileMin tileMax)

imagePixels :: String -> Int -> Int -> Int -> Int -> [(Int, Int, Int)] -> IO BS.ByteString
imagePixels key minimumValue maximumValue tx ty points =
  BSI.create (imageTileSize * imageTileSize * 4) $ \pixels -> do
    Storable.unsafeWith xs $ \xPtr ->
      Storable.unsafeWith ys $ \yPtr ->
        Storable.unsafeWith values $ \valuePtr ->
          c_rgbaTile (Storable.length values) xPtr yPtr valuePtr tx ty minimumValue maximumValue bankLayer pixels
 where
  xs = Storable.fromList [x | (x, _, _) <- points]
  ys = Storable.fromList [y | (_, y, _) <- points]
  values = Storable.fromList [value | (_, _, value) <- points]
  bankLayer = if key == "bank" then 1 else 0

imageTileSize :: Int
imageTileSize = 256

imageTileKey :: Int -> Int -> Int -> Int
imageTileKey plane tx ty = (plane `shiftL` 58) .|. ((tx .&. tileCoordMask) `shiftL` 29) .|. (ty .&. tileCoordMask)

decodeImageTileKey :: Int -> (Int, Int, Int)
decodeImageTileKey encoded = (encoded `shiftR` 58, (encoded `shiftR` 29) .&. tileCoordMask, encoded .&. tileCoordMask)

tileCoordMask :: Int
tileCoordMask = (1 `shiftL` 29) - 1

heuristicAt :: NaturalComponents -> Heuristic -> State -> Int
heuristicAt components heuristic (State tile banked) =
  case componentOf components tile of
    Nothing -> 0
    Just cid ->
      let seeds = heuristicSeeds heuristic Boxed.! seedKey cid banked
       in if Vector.null seeds
            then 0
            else finite (Vector.minimum (Vector.map seedDistance seeds))
 where
  seedDistance (packed, cost) = addCostDefault maxBound cost (chebyshevPacked (unTile tile) packed)

seedTableFromDistances :: NaturalComponents -> SiteGraph -> Vector.Vector Int -> Boxed.Vector (Vector.Vector (Int, Int))
seedTableFromDistances components graph distances = runST $ do
  table <- Boxed.thaw emptySeedLists
  Vector.iforM_ (siteTiles graph) $ \node packed -> do
    let cid = siteComponents graph Vector.! node
    when (cid >= 0) $ do
      addSeed table cid False packed (distances Vector.! stateId node False)
      addSeed table cid True packed (distances Vector.! stateId node True)
  lists <- Boxed.freeze table
  pure (Boxed.map Vector.fromList lists)
 where
  emptySeedLists = Boxed.replicate ((maxComponentId components + 1) * 2) []
  addSeed table cid banked packed distance =
    when (distance /= maxBound) $ do
      seeds <- BoxedMutable.read table (seedKey cid banked)
      BoxedMutable.write table (seedKey cid banked) ((packed, distance) : seeds)

seedKey :: Int -> Bool -> Int
seedKey cid banked = cid * 2 + if banked then 1 else 0

siteGraph :: TileAStar -> Query -> SiteGraph
siteGraph (TileAStar world components static) q =
  SiteGraph tiles tileIndex comps staticCount (staticWalkingNetwork static) componentSites reverseEdges
 where
  staticCount = Vector.length (staticTiles static)
  queryExtras =
    [ packed
    | packed <- IntSet.toAscList (IntSet.fromList [unTile (queryStart q), unTile (queryTarget q)])
    , binarySearch packed (staticTiles static) == Nothing
    ]
  tiles = staticTiles static <> Vector.fromList queryExtras
  tileIndex = IntMap.fromList [(packed, ix) | (ix, packed) <- Vector.toList (Vector.indexed tiles)]
  extraComps = Vector.fromList [maybe (-1) id (componentOf components (Tile packed)) | packed <- queryExtras]
  comps = staticComponents static <> extraComps
  componentSites = siteComponentGroups (maxComponentId components) comps
  nodeCount = Vector.length tiles
  edges = localEdges <> bankEdges <> initialGlobalEdges <> bankGlobalEdges
  reverseEdges = reverseAdjacency (nodeCount * 2) edges

  localEdges =
    [ (stateId from banked, stateId to banked, transportCost t)
    | allowTransports q
    , banked <- [False, True]
    , transports <- Map.elems (worldTransports world)
    , t <- transports
    , transportAvailable q banked t
    , Just originTile <- [origin t]
    , Just destinationTile <- [destination t]
    , Just from <- [nodeFor originTile]
    , Just to <- [nodeFor destinationTile]
    ]

  bankEdges =
    [ (stateId node False, stateId node True, 0)
    | bankPathEnabled q
    , tile <- Set.toList (worldBanks world)
    , Just node <- [nodeFor tile]
    ]

  initialGlobalEdges =
    [ (stateId from False, stateId to False, transportCost t)
    | allowTransports q
    , t <- worldGlobalTeleports world
    , transportAvailable q False t
    , Just from <- [nodeFor (queryStart q)]
    , Just destinationTile <- [destination t]
    , Just to <- [nodeFor destinationTile]
    ]

  bankGlobalEdges =
    [ (stateId from True, stateId to True, transportCost t)
    | allowTransports q
    , bankPathEnabled q
    , bank <- Set.toList (worldBanks world)
    , Just from <- [nodeFor bank]
    , t <- worldGlobalTeleports world
    , transportAvailable q True t
    , Just destinationTile <- [destination t]
    , Just to <- [nodeFor destinationTile]
    ]

  nodeFor tile = IntMap.lookup (unTile tile) tileIndex
  transportCost t = duration t + Map.findWithDefault 0 (transportType t) (transportPenalties q)

siteComponentGroups :: Int -> Vector.Vector Int -> Boxed.Vector (Vector.Vector Int)
siteComponentGroups highestComponent comps = runST $ do
  groups <- BoxedMutable.replicate (highestComponent + 1) []
  Vector.iforM_ comps $ \node cid ->
    when (cid >= 0) $ do
      nodes <- BoxedMutable.read groups cid
      BoxedMutable.write groups cid (node : nodes)
  frozen <- Boxed.freeze groups
  pure (Boxed.map Vector.fromList frozen)

targetSeeds :: SiteGraph -> Tile -> [(Int, Int)]
targetSeeds graph target =
  [ (stateId node banked, 0)
  | Just node <- [IntMap.lookup (unTile target) (siteTileIndex graph)]
  , banked <- [False, True]
  ]

targetSeedsManhattan :: SiteGraph -> Tile -> [(Int, Int)]
targetSeedsManhattan = targetSeeds

reverseDijkstra :: SiteGraph -> [(Int, Int)] -> (Vector.Vector Int, TileReverseCounters)
reverseDijkstra graph seeds = runST $ do
  result <- Mutable.replicate stateCount maxBound
  queue <- queueNew (max 262144 (stateCount * 16))
  counters <- foldM (seed result queue) emptyReverseCounters seeds
  let searchReverse currentCounters = do
        popped <- queuePop queue
        case popped of
          Nothing -> do
            distances <- Vector.freeze result
            pure (distances, currentCounters)
          Just (_, node, cost) -> do
            let poppedCounters = currentCounters
                  { reversePqPops = reversePqPops currentCounters + 1
                  }
            known <- Mutable.read result node
            if cost /= known
              then searchReverse poppedCounters {reverseStalePqEntries = reverseStalePqEntries poppedCounters + 1}
              else do
                transportCounters <- Vector.foldM (relax result queue cost True) (poppedCounters {reverseStatesPopped = reverseStatesPopped poppedCounters + 1}) (siteReverseEdges graph Boxed.! node)
                scanCounters <- relaxSameComponent result queue cost node transportCounters
                searchReverse scanCounters
  searchReverse counters
 where
  siteCount = Vector.length (siteTiles graph)
  stateCount = siteCount * 2
  seed :: Mutable.MVector s Int -> MutableQueue s -> TileReverseCounters -> (Int, Int) -> ST s TileReverseCounters
  seed result queue counters (node, cost) = do
    Mutable.write result node cost
    queuePush queue cost node cost
    pure counters {reversePqPushes = reversePqPushes counters + 1}
  relax :: Mutable.MVector s Int -> MutableQueue s -> Int -> Bool -> TileReverseCounters -> (Int, Int) -> ST s TileReverseCounters
  relax result queue cost transportEdge counters (next, edgeCost) =
    case addCost cost edgeCost of
      Nothing -> pure counters'
      Just newCost -> do
        known <- Mutable.read result next
        if newCost >= known
          then pure counters'
          else do
            Mutable.write result next newCost
            queuePush queue newCost next newCost
            pure counters' {reversePqPushes = reversePqPushes counters' + 1}
   where
    counters'
      | transportEdge = counters {reverseTransportRelaxations = reverseTransportRelaxations counters + 1}
      | otherwise = counters
  relaxSameComponent :: Mutable.MVector s Int -> MutableQueue s -> Int -> Int -> TileReverseCounters -> ST s TileReverseCounters
  relaxSameComponent result queue cost node counters =
    go 0 counters'
     where
      site = node `div` 2
      banked = odd node
      sourceTile = siteTiles graph Vector.! site
      sourceComponent = siteComponents graph Vector.! site
      sameComponentSites
        | sourceComponent < 0 = Vector.empty
        | otherwise = siteComponentSiteIds graph Boxed.! sourceComponent
      scanned = Vector.length sameComponentSites
      counters' = counters
        { reverseSameComponentSiteScans = reverseSameComponentSiteScans counters + 1
        , reverseTotalSitesScanned = reverseTotalSitesScanned counters + scanned
        , reverseMaxSitesScannedPerPop = max (reverseMaxSitesScannedPerPop counters) scanned
        }
      go ix countersSoFar
        | ix >= scanned = pure countersSoFar
        | other == site = go (ix + 1) countersSoFar
        | otherwise = do
            nextCounters <- relax result queue cost False scanCounters (stateId other banked, chebyshevPacked sourceTile otherTile)
            go (ix + 1) nextCounters
       where
        other = sameComponentSites Vector.! ix
        otherTile = siteTiles graph Vector.! other
        scanCounters = countersSoFar {reverseChebyshevComparisons = reverseChebyshevComparisons countersSoFar + 1}

reverseDijkstraUncounted :: SiteGraph -> [(Int, Int)] -> Vector.Vector Int
reverseDijkstraUncounted graph seeds = runST $ do
  result <- Mutable.replicate stateCount maxBound
  queue <- queueNew (max 262144 (stateCount * 16))
  forM_ seeds (seed result queue)
  let searchReverse = do
        popped <- queuePop queue
        case popped of
          Nothing -> Vector.freeze result
          Just (_, node, cost) -> do
            known <- Mutable.read result node
            if cost /= known
              then searchReverse
              else do
                Vector.forM_ (siteReverseEdges graph Boxed.! node) (relax result queue cost)
                relaxSameComponent result queue cost node
                searchReverse
  searchReverse
 where
  siteCount = Vector.length (siteTiles graph)
  stateCount = siteCount * 2
  seed :: Mutable.MVector s Int -> MutableQueue s -> (Int, Int) -> ST s ()
  seed result queue (node, cost) = do
    Mutable.write result node cost
    queuePush queue cost node cost
  relax :: Mutable.MVector s Int -> MutableQueue s -> Int -> (Int, Int) -> ST s ()
  relax result queue cost (next, edgeCost) =
    case addCost cost edgeCost of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read result next
        when (newCost < known) $ do
          Mutable.write result next newCost
          queuePush queue newCost next newCost
  relaxSameComponent :: Mutable.MVector s Int -> MutableQueue s -> Int -> Int -> ST s ()
  relaxSameComponent result queue cost node =
    go 0
   where
    site = node `div` 2
    banked = odd node
    sourceTile = siteTiles graph Vector.! site
    sourceComponent = siteComponents graph Vector.! site
    sameComponentSites
      | sourceComponent < 0 = Vector.empty
      | otherwise = siteComponentSiteIds graph Boxed.! sourceComponent
    scanned = Vector.length sameComponentSites
    go ix
      | ix >= scanned = pure ()
      | other == site = go (ix + 1)
      | otherwise = do
          relax result queue cost (stateId other banked, chebyshevPacked sourceTile otherTile)
          go (ix + 1)
     where
      other = sameComponentSites Vector.! ix
      otherTile = siteTiles graph Vector.! other

reverseDijkstraManhattanUncounted :: SiteGraph -> [(Int, Int)] -> Vector.Vector Int
reverseDijkstraManhattanUncounted graph seeds = runST $ do
  result <- Mutable.replicate stateCount maxBound
  queue <- queueNew (max 262144 (stateCount * 16))
  forM_ seeds (seed result queue)
  let searchReverse = do
        popped <- queuePop queue
        case popped of
          Nothing -> Vector.freeze result
          Just (_, node, cost) -> do
            known <- Mutable.read result node
            if cost /= known
              then searchReverse
              else do
                relaxWalking result queue cost node
                when (node `div` 2 < siteCount) $
                  Vector.forM_ (siteReverseEdges graph Boxed.! node) (relax result queue cost . doubleEdge)
                searchReverse
  searchReverse
 where
  siteCount = Vector.length (siteTiles graph)
  staticCount = siteStaticCount graph
  network = siteSparseNetwork graph
  stateCount = (siteCount + sparseSteinerCount network) * 2
  seed :: Mutable.MVector s Int -> MutableQueue s -> (Int, Int) -> ST s ()
  seed result queue (node, cost) = do
    Mutable.write result node (cost * 2)
    queuePush queue (cost * 2) node (cost * 2)
  doubleEdge (next, edgeCost) = (next, edgeCost * 2)
  relax :: Mutable.MVector s Int -> MutableQueue s -> Int -> (Int, Int) -> ST s ()
  relax result queue cost (next, edgeCost) =
    case addCost cost edgeCost of
      Nothing -> pure ()
      Just newCost -> do
        known <- Mutable.read result next
        when (newCost < known) $ do
          Mutable.write result next newCost
          queuePush queue newCost next newCost
  relaxWalking :: Mutable.MVector s Int -> MutableQueue s -> Int -> Int -> ST s ()
  relaxWalking result queue cost state = do
    relaxSparseWalkingEdges result queue cost banked vertex
    when (vertex < siteCount) (relaxQueryAttachments result queue cost vertex banked)
   where
    vertex = state `div` 2
    banked = odd state
  relaxSparseWalkingEdges :: Mutable.MVector s Int -> MutableQueue s -> Int -> Bool -> Int -> ST s ()
  relaxSparseWalkingEdges result queue cost banked vertex
    | vertex < staticCount = go (sparseOffsets network Vector.! vertex)
    | vertex < siteCount = pure ()
    | otherwise = go (sparseOffsets network Vector.! sparseVertex)
   where
    sparseVertex = staticCount + vertex - siteCount
    end
      | vertex < staticCount = sparseOffsets network Vector.! (vertex + 1)
      | otherwise = sparseOffsets network Vector.! (sparseVertex + 1)
    go ix
      | ix >= end = pure ()
      | otherwise = do
          let sparseNext = sparseDestinations network Vector.! ix
              edgeCost = sparseWeights network Vector.! ix
              next
                | sparseNext < staticCount = sparseNext
                | otherwise = siteCount + sparseNext - staticCount
          relax result queue cost (stateId next banked, edgeCost)
          go (ix + 1)
  relaxQueryAttachments :: Mutable.MVector s Int -> MutableQueue s -> Int -> Int -> Bool -> ST s ()
  relaxQueryAttachments result queue cost vertex banked =
    go 0
   where
    vertexTile = siteTiles graph Vector.! vertex
    vertexComponent = siteComponents graph Vector.! vertex
    componentSites
      | vertexComponent < 0 = Vector.empty
      | otherwise = siteComponentSiteIds graph Boxed.! vertexComponent
    count = Vector.length componentSites
    go ix
      | ix >= count = pure ()
      | other == vertex = go (ix + 1)
      | vertex < staticCount && other < staticCount = go (ix + 1)
      | otherwise = do
          relax result queue cost (stateId other banked, chebyshevPacked vertexTile otherTile * 2)
          go (ix + 1)
     where
      other = componentSites Vector.! ix
      otherTile = siteTiles graph Vector.! other

halveDistances :: Vector.Vector Int -> Vector.Vector Int
halveDistances = Vector.map halve
 where
  halve value
    | value == maxBound = maxBound
    | otherwise = value `div` 2

assertReverseLabelsEqual :: SiteGraph -> Vector.Vector Int -> Vector.Vector Int -> IO ()
assertReverseLabelsEqual graph clique manhattan =
  case [ (state, clique Vector.! state, manhattan Vector.! state)
       | node <- [0 .. Vector.length (siteTiles graph) - 1]
       , banked <- [False, True]
       , let state = stateId node banked
       , clique Vector.! state /= manhattan Vector.! state
       ] of
    [] -> pure ()
    ((state, expected, actual):_) ->
      fail ("sparse Manhattan reverse label mismatch at state " <> show state <> ": clique=" <> show expected <> " manhattan=" <> show actual)

chebyshevTransform :: Box -> [(Tile, Int)] -> Vector.Vector Int
chebyshevTransform box seeds = runST $ do
  values <- Mutable.replicate size maxBound
  forM_ seeds $ \(tile, cost) ->
    case offset box tile of
      Nothing -> pure ()
      Just index -> do
        old <- Mutable.read values index
        when (cost < old) (Mutable.write values index cost)
  forwardRows values 0
  backwardRows values (height - 1)
  Vector.freeze values
 where
  width = boxMaxX box - boxMinX box + 1
  height = boxMaxY box - boxMinY box + 1
  size = width * height
  ix x y = y * width + x

  forwardRows :: Mutable.MVector s Int -> Int -> ST s ()
  forwardRows values y
    | y >= height = pure ()
    | otherwise = forwardColumns values y 0 >> forwardRows values (y + 1)

  forwardColumns :: Mutable.MVector s Int -> Int -> Int -> ST s ()
  forwardColumns values y x
    | x >= width = pure ()
    | otherwise = do
        let here = ix x y
        current <- Mutable.read values here
        best0 <- bestNeighbour values current (x - 1) (y - 1)
        best1 <- bestNeighbour values best0 x (y - 1)
        best2 <- bestNeighbour values best1 (x + 1) (y - 1)
        best3 <- bestNeighbour values best2 (x - 1) y
        when (best3 < current) (Mutable.write values here best3)
        forwardColumns values y (x + 1)

  backwardRows :: Mutable.MVector s Int -> Int -> ST s ()
  backwardRows values y
    | y < 0 = pure ()
    | otherwise = backwardColumns values y (width - 1) >> backwardRows values (y - 1)

  backwardColumns :: Mutable.MVector s Int -> Int -> Int -> ST s ()
  backwardColumns values y x
    | x < 0 = pure ()
    | otherwise = do
        let here = ix x y
        current <- Mutable.read values here
        best0 <- bestNeighbour values current (x + 1) y
        best1 <- bestNeighbour values best0 (x - 1) (y + 1)
        best2 <- bestNeighbour values best1 x (y + 1)
        best3 <- bestNeighbour values best2 (x + 1) (y + 1)
        when (best3 < current) (Mutable.write values here best3)
        backwardColumns values y (x - 1)

  bestNeighbour :: Mutable.MVector s Int -> Int -> Int -> Int -> ST s Int
  bestNeighbour values best x y
    | x < 0 || x >= width || y < 0 || y >= height = pure best
    | otherwise = do
        value <- Mutable.read values (ix x y)
        pure (min best (addCostDefault maxBound value 1))

chebyshevTransformC :: Box -> [(Tile, Int)] -> Vector.Vector Int
chebyshevTransformC box seeds =
  Generic.convert $
    unsafePerformIO $
      Storable.unsafeWith seedOffsets $ \offsetPtr ->
        Storable.unsafeWith seedCosts $ \costPtr ->
          do
            values <- mallocForeignPtrArray size
            withForeignPtr values $ \valuePtr ->
              c_chebyshevTransform width height (Storable.length seedOffsets) offsetPtr costPtr valuePtr
            pure (Storable.unsafeFromForeignPtr0 values size)
 where
  width = boxMaxX box - boxMinX box + 1
  height = boxMaxY box - boxMinY box + 1
  size = width * height
  validSeeds =
    [ (ix, cost)
    | (tile, cost) <- seeds
    , Just ix <- [offset box tile]
    ]
  seedOffsets = Storable.fromList (map fst validSeeds)
  seedCosts = Storable.fromList (map snd validSeeds)

chebyshevTransformSlow :: Box -> [(Tile, Int)] -> Vector.Vector Int
chebyshevTransformSlow box seeds = Vector.generate size valueAt
 where
  width = boxMaxX box - boxMinX box + 1
  height = boxMaxY box - boxMinY box + 1
  size = width * height
  valueAt ix =
    let (dy, dx) = ix `divMod` width
        tile = packTile (boxMinX box + dx) (boxMinY box + dy) (boxPlane box)
     in minimumDefault maxBound [addCostDefault maxBound cost distance | (seed, cost) <- seeds, Just distance <- [chebyshev2 tile seed]]

naturalComponents :: World -> IO NaturalComponents
naturalComponents world = do
  let walkable = IntSet.fromList (map unTile (collisionTiles (worldCollision world)))
  queue <- Mutable.new (max 1 (IntSet.size walkable))
  owner <- go queue 1 walkable IntMap.empty
  let reachable = reachableComponents world owner
      pairs = [(tile, cid) | (tile, cid) <- IntMap.toAscList owner, IntSet.member cid reachable]
      ids = IntSet.toAscList (IntSet.fromList (map snd pairs))
  pure
    NaturalComponents
      { componentOwnerTiles = Vector.fromList (map fst pairs)
      , componentOwnerIds = Vector.fromList (map snd pairs)
      , componentIds = Vector.fromList ids
      , maxComponentId = maximumDefault 0 ids
      }
 where
  go queue cid remaining owner =
    case IntSet.minView remaining of
      Nothing -> pure owner
      Just (start, rest) -> do
        Mutable.write queue 0 start
        (owner', rest') <- flood queue cid rest (IntMap.insert start cid owner) 0 1
        go queue (cid + 1) rest' owner'

  flood queue cid remaining owner readIx writeIx
    | readIx == writeIx = pure (owner, remaining)
    | otherwise = do
        packed <- Mutable.read queue readIx
        let next =
              [ unTile tile
              | tile <- walkingNeighborsRaw world (Tile packed)
              , IntSet.member (unTile tile) remaining
              ]
            remaining' = foldr IntSet.delete remaining next
            owner' = foldr (`IntMap.insert` cid) owner next
        forM_ (zip [writeIx ..] next) (uncurry (Mutable.write queue))
        flood queue cid remaining' owner' (readIx + 1) (writeIx + length next)

reachableComponents :: World -> IntMap.IntMap Int -> IntSet.IntSet
reachableComponents world owner =
  case IntMap.lookup (unTile (packTile 3221 3218 0)) owner of
    Nothing -> IntSet.fromList (IntMap.elems owner)
    Just start -> close (IntSet.singleton start) [start]
 where
  localEdges =
    [ (a, b)
    | transport <- concat (Map.elems (worldTransports world))
    , transportType transport /= "VIRTUAL_WALL"
    , Just originTile <- [origin transport]
    , Just destinationTile <- [destination transport]
    , a <- componentsAt originTile
    , b <- componentsAt destinationTile
    ]
  globalDestinations =
    IntSet.fromList
      [ component
      | transport <- worldGlobalTeleports world
      , Just destinationTile <- [destination transport]
      , component <- componentsAt destinationTile
      ]
  componentsAt tile = IntSet.toList (IntSet.fromList
    [ component
    | candidate <- tile : walkingNeighborsRaw world tile
    , Just component <- [IntMap.lookup (unTile candidate) owner]
    ])
  close seen [] = seen
  close seen (component:rest) =
    let next = [b | (a, b) <- localEdges, a == component] <> IntSet.toList globalDestinations
        fresh = filter (`IntSet.notMember` seen) next
     in close (foldr IntSet.insert seen fresh) (fresh <> rest)

componentOf :: NaturalComponents -> Tile -> Maybe Int
componentOf components tile = (componentOwnerIds components Vector.!?) =<< binarySearch (unTile tile) (componentOwnerTiles components)

offset :: Box -> Tile -> Maybe Int
offset box tile =
  let (x, y, p) = unpackTile tile
      width = boxMaxX box - boxMinX box + 1
   in if p == boxPlane box && x >= boxMinX box && x <= boxMaxX box && y >= boxMinY box && y <= boxMaxY box
        then Just ((y - boxMinY box) * width + (x - boxMinX box))
        else Nothing

stateId :: Int -> Bool -> Int
stateId node banked = node * 2 + if banked then 1 else 0

finite :: Int -> Int
finite value
  | value == maxBound = 0
  | otherwise = value

addCost :: Int -> Int -> Maybe Int
addCost a b
  | a == maxBound || b == maxBound || b < 0 || a > maxBound - b = Nothing
  | otherwise = Just (a + b)

addCostDefault :: Int -> Int -> Int -> Int
addCostDefault fallback a b = maybe fallback id (addCost a b)

minimumDefault :: Ord a => a -> [a] -> a
minimumDefault fallback [] = fallback
minimumDefault _ values = minimum values

timedIO :: (a -> IO b) -> IO a -> IO (a, Double)
timedIO force action = do
  started <- getMonotonicTimeNSec
  value <- action
  _ <- force value
  finished <- getMonotonicTimeNSec
  pure (value, milliseconds started finished)

forceHeuristic :: Heuristic -> IO Int
forceHeuristic heuristic =
  evaluate
    ( Boxed.foldl' (\total seeds -> total + Vector.length seeds) 0 (heuristicSeeds heuristic)
        + round (heuristicReverseMilliseconds heuristic)
        + round (heuristicSeedTableMilliseconds heuristic)
        + reverseStatesPopped (heuristicReverseCounters heuristic)
    )

forceReverseResult :: (Vector.Vector Int, TileReverseCounters) -> IO Int
forceReverseResult (distances, counters) =
  evaluate
    ( Vector.sum distances
        + reverseStatesPopped counters
        + reverseStalePqEntries counters
        + reversePqPushes counters
        + reversePqPops counters
        + reverseSameComponentSiteScans counters
        + reverseTotalSitesScanned counters
        + reverseChebyshevComparisons counters
        + reverseMaxSitesScannedPerPop counters
        + reverseTransportRelaxations counters
    )

forceSeedTable :: Boxed.Vector (Vector.Vector (Int, Int)) -> IO Int
forceSeedTable table =
  evaluate (Boxed.ifoldl' (\total ix seeds -> total + ix + Vector.length seeds) 0 table)

forceSearch :: (Route, TileAStarCounters) -> IO Int
forceSearch (route, counters) =
  evaluate
    ( routeCost route
        + routeExpandedNodes route
        + length (routeSteps route)
        + tileStatesPopped counters
        + tileStalePqEntries counters
        + tilePqPushes counters
        + tileUniqueStatesReached counters
        + tileWalkingRelaxations counters
        + tileTransportRelaxations counters
        + tileHeuristicEvaluations counters
    )

forcePointList :: [(Int, Int)] -> IO Int
forcePointList points =
  evaluate (foldl' (\total (packed, value) -> total + packed + value) 0 points)

milliseconds :: Word64 -> Word64 -> Double
milliseconds started finished = fromIntegral (finished - started) / 1000000

reverseAdjacency :: Int -> [(Int, Int, Int)] -> Boxed.Vector (Vector.Vector (Int, Int))
reverseAdjacency size edges = runST $ do
    lists <- Boxed.thaw (Boxed.replicate size [])
    forM_ edges $ \(from, to, cost) -> do
      current <- BoxedMutable.read lists to
      BoxedMutable.write lists to ((from, cost) : current)
    frozen <- Boxed.freeze lists
    pure (Boxed.map Vector.fromList frozen)

binarySearch :: Int -> Vector.Vector Int -> Maybe Int
binarySearch needle values = go 0 (Vector.length values - 1)
 where
  go lo hi
    | lo > hi = Nothing
    | current == needle = Just mid
    | current < needle = go (mid + 1) hi
    | otherwise = go lo (mid - 1)
   where
    mid = (lo + hi) `div` 2
    current = values Vector.! mid

chebyshevPacked :: Int -> Int -> Int
chebyshevPacked a b =
  let (ax, ay, ap) = unpackTile (Tile a)
      (bx, by, bp) = unpackTile (Tile b)
   in if ap == bp then max (abs (ax - bx)) (abs (ay - by)) else maxBound

maximumDefault :: Ord a => a -> [a] -> a
maximumDefault fallback [] = fallback
maximumDefault _ values = maximum values
