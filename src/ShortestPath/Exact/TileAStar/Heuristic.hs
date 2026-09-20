{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ShortestPath.Exact.TileAStar.Heuristic
  ( Heuristic(..)
  , PreparedTarget(..)
  , ManhattanHeuristicMode(..)
  , ReverseImplementation(..)
  , TileAStarConfig(..)
  , defaultTileAStarConfig
  , heuristicAt
  , heuristicAtComponent
  , heuristicAtGeneratorsComponent
  , heuristicAtComponentRaw
  , heuristicAtComponentCountedRaw
  , heuristicAtResolved
  , heuristicAtResolvedRaw
  , heuristicAtResolvedCountedRaw
  , heuristicFromDistances
  , prepareHeuristic
  , prepareHeuristicFor
  , prepareHeuristicProfiled
  , prepareHeuristicProfiledFor
  , seedKey
  ) where

import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Bits ((.&.), shiftR)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Unboxed as Vector
import Control.Monad.ST (ST, runST)
import Data.List (sort)
import GHC.Exts (Int(I#), Int#)

import ShortestPath.Exact.TileAStar.RelaxedGraph
import ShortestPath.Exact.TileAStar.HeuristicScan
import ShortestPath.Exact.TileAStar.ReverseSearch
import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.Timing
import ShortestPath.Tile
import ShortestPath.Topology

data ReverseImplementation = CliqueReverse | SparseWalkingReverse
  deriving stock (Eq, Show)

data TileAStarConfig = TileAStarConfig
  { tileReverseImplementation :: !ReverseImplementation
  , tileManhattanHeuristicMode :: !ManhattanHeuristicMode
  , tileCompareReverseImplementations :: !Bool
  , tileCollectReverseCounters :: !Bool
  }
  deriving stock (Eq, Show)

defaultTileAStarConfig :: TileAStarConfig
defaultTileAStarConfig = TileAStarConfig SparseWalkingReverse ManhattanSeedScan False False

data GatewayHeuristic = GatewayHeuristic
  { gatewayNetwork :: SparseWalkingNetwork
  , gatewayDistances :: Vector.Vector Int
  , gatewayReverseNodeCount :: !Int
  , gatewayStaticCount :: !Int
  , gatewayTargetPacked :: !Int
  , gatewayTargetComponents :: Vector.Vector Int
  }

data Heuristic = Heuristic
  { heuristicSeeds :: Boxed.Vector (Vector.Vector (Int, Int))
  , heuristicGenerators :: Boxed.Vector (Vector.Vector (Int, Int))
  , heuristicGeneratorScans :: Boxed.Vector GeneratorScan
  , heuristicSiteIndex :: IntMap.IntMap Int
  , heuristicSiteDistances :: Vector.Vector Int
  , heuristicGatewayData :: Maybe GatewayHeuristic
  , heuristicReverseMilliseconds :: !Double
  , heuristicSeedTableMilliseconds :: !Double
  , heuristicReverseCounters :: !TileReverseCounters
  , heuristicSeedCount :: !Int
  , heuristicComponentCount :: !Int
  , heuristicMaxSeedsPerComponent :: !Int
  , heuristicSeedsPerComponentP50 :: !Int
  , heuristicSeedsPerComponentP90 :: !Int
  , heuristicSeedsPerComponentP95 :: !Int
  , heuristicSeedsPerComponentP99 :: !Int
  , heuristicGeneratorCount :: !Int
  , heuristicMaxGeneratorsPerComponent :: !Int
  , heuristicGeneratorsPerComponentP50 :: !Int
  , heuristicGeneratorsPerComponentP90 :: !Int
  , heuristicGeneratorsPerComponentP95 :: !Int
  , heuristicGeneratorsPerComponentP99 :: !Int
  , heuristicGeneratorSeedRatioP50 :: !Double
  , heuristicGeneratorSeedRatioP90 :: !Double
  , heuristicGeneratorSeedRatioP95 :: !Double
  , heuristicGeneratorSeedRatioP99 :: !Double
  , heuristicGeneratorSeedRatioMax :: !Double
  }

data PreparedTarget = PreparedTarget
  { preparedTargetTile :: !Tile
  , preparedTargetOverlay :: TargetOverlay
  , preparedSearchExtraTiles :: Vector.Vector Int
  , preparedSearchExtraComponents :: Boxed.Vector (Vector.Vector Int)
  , preparedSearchExtraSites :: Vector.Vector Int
  , preparedTargetHeuristic :: Heuristic
  }

-- | The production, untimed heuristic used by the pure Tile A* entry point.
prepareHeuristic :: TileAStar -> CompiledRoutingAccount -> Tile -> Heuristic
prepareHeuristic astar account target =
  prepareHeuristicFor astar account (targetOverlay astar account target)

prepareHeuristicFor :: TileAStar -> CompiledRoutingAccount -> TargetOverlay -> Heuristic
prepareHeuristicFor astar account overlay =
  heuristicFromDistances components graph overlay distances 0 0 emptyReverseCounters
 where
  graph = compiledSiteGraph account
  distances = halveDistances (manhattanDistances (reverseDijkstraManhattanUncounted graph overlay))
  components = topologyRoutingComponents (tileTopology astar)

prepareHeuristicProfiled :: TileAStarConfig -> TileAStar -> CompiledRoutingAccount -> Tile -> IO Heuristic
prepareHeuristicProfiled config astar account target =
  prepareHeuristicProfiledFor config astar account (targetOverlayForMode (tileManhattanHeuristicMode config) astar account target)

prepareHeuristicProfiledFor :: TileAStarConfig -> TileAStar -> CompiledRoutingAccount -> TargetOverlay -> IO Heuristic
prepareHeuristicProfiledFor config astar account overlay
  | tileManhattanHeuristicMode config == ManhattanGateways = prepareGateways
  | otherwise = prepareSeedScan
 where
  graph = compiledSiteGraph account
  components = topologyRoutingComponents (tileTopology astar)
  prepareGateways = case tileReverseImplementation config of
    CliqueReverse -> fail "ManhattanGateways requires SparseWalkingReverse"
    SparseWalkingReverse -> do
      ((result, counters), reverseMs) <- timedIO forceGatewayReverseResult gatewayReverseAction
      let distances = halveDistances (manhattanDistances result)
      compareReverse distances
      pure (heuristicFromGatewayDistances components graph overlay result reverseMs counters)
  gatewayReverseAction
    | tileCollectReverseCounters config = pure (reverseDijkstraManhattanGateways graph overlay)
    | otherwise = pure (reverseDijkstraManhattanGatewaysUncounted graph overlay, emptyReverseCounters)
  prepareSeedScan = do
    ((distances, counters, provenance), reverseMs) <- timedIO forceReverseResult seedReverseAction
    ((table, generators), seedMs) <- timedIO forceSeedTables (pure (case provenance of
      Nothing -> (seedTableFromDistances components graph overlay distances, emptyGeneratorTable components)
      Just result -> seedTablesFromManhattanResult components graph overlay distances result))
    pure (heuristicFromSeedTables table generators graph overlay distances reverseMs seedMs counters)
  seedReverseAction = case tileReverseImplementation config of
    CliqueReverse
      | tileCollectReverseCounters config ->
          let (distances, counters) = reverseDijkstra graph overlay
           in pure (distances, counters, Nothing)
      | otherwise -> pure (reverseDijkstraUncounted graph overlay, emptyReverseCounters, Nothing)
    SparseWalkingReverse
      | tileCollectReverseCounters config -> do
          let (result, counters) = reverseDijkstraManhattan graph overlay
              distances = halveDistances (manhattanDistances result)
          compareReverse distances
          pure (distances, counters, Just result)
      | otherwise -> do
          let result = reverseDijkstraManhattanUncounted graph overlay
              distances = halveDistances (manhattanDistances result)
          compareReverse distances
          pure (distances, emptyReverseCounters, Just result)
  compareReverse distances =
    when (tileCompareReverseImplementations config)
      (assertReverseLabelsEqual graph comparisonOverlay (reverseDijkstraUncounted graph comparisonOverlay) distances)
  comparisonOverlay
    | tileManhattanHeuristicMode config == ManhattanGateways = withLegacyTargetAttachments graph overlay
    | otherwise = overlay

heuristicAt :: WorldTopology -> Heuristic -> Tile -> Bool -> Maybe Int
{-# INLINE heuristicAt #-}
heuristicAt topology heuristic tile banked =
  heuristicAtResolved heuristic packed attachments site banked
 where
  packed = unTile tile
  attachments = Vector.fromList (routingPointAttachments topology tile)
  site = IntMap.findWithDefault (-1) packed (heuristicSiteIndex heuristic)

heuristicAtComponent :: Heuristic -> Int -> Int -> Bool -> Maybe Int
{-# INLINE heuristicAtComponent #-}
heuristicAtComponent heuristic packed cid banked =
  finite (heuristicAtComponentRaw heuristic packed cid banked)

heuristicAtGeneratorsComponent :: Heuristic -> Int -> Int -> Bool -> Maybe Int
heuristicAtGeneratorsComponent heuristic packed cid banked =
  finite (candidateDistance (heuristicGenerators heuristic Boxed.! seedKey cid banked) packed)

heuristicAtComponentRaw :: Heuristic -> Int -> Int -> Bool -> Int
{-# INLINE heuristicAtComponentRaw #-}
heuristicAtComponentRaw = componentDistance

heuristicAtComponentCountedRaw :: Heuristic -> Int -> Int -> Bool -> (# Int#, Int# #)
{-# INLINE heuristicAtComponentCountedRaw #-}
heuristicAtComponentCountedRaw = componentDistanceCounted

heuristicAtResolved :: Heuristic -> Int -> Vector.Vector Int -> Int -> Bool -> Maybe Int
{-# INLINE heuristicAtResolved #-}
heuristicAtResolved heuristic packed components site banked =
  finite (heuristicAtResolvedRaw heuristic packed components site banked)

heuristicAtResolvedRaw :: Heuristic -> Int -> Vector.Vector Int -> Int -> Bool -> Int
{-# INLINE heuristicAtResolvedRaw #-}
heuristicAtResolvedRaw heuristic packed components site banked =
  Vector.foldl' (\best cid -> min best (componentDistance heuristic packed cid banked)) exact components
 where
  exact
    | site < 0 = maxBound
    | otherwise = heuristicSiteDistances heuristic Vector.! stateId site banked

heuristicAtResolvedCountedRaw :: Heuristic -> Int -> Vector.Vector Int -> Int -> Bool -> (# Int#, Int# #)
{-# INLINE heuristicAtResolvedCountedRaw #-}
heuristicAtResolvedCountedRaw heuristic packed components site banked = go 0 exact 0
 where
  exact
    | site < 0 = maxBound
    | otherwise = heuristicSiteDistances heuristic Vector.! stateId site banked
  go ix best scanned
    | ix >= Vector.length components = case (best, scanned) of
        (I# best#, I# scanned#) -> (# best#, scanned# #)
    | otherwise = case componentDistanceCounted heuristic packed (components Vector.! ix) banked of
        (# distance#, count# #) -> go (ix + 1) (min best (I# distance#)) (scanned + I# count#)

componentDistance :: Heuristic -> Int -> Int -> Bool -> Int
{-# INLINE componentDistance #-}
componentDistance heuristic packed cid banked =
  case heuristicGatewayData heuristic of
    Nothing -> candidateDistance (heuristicSeeds heuristic Boxed.! seedKey cid banked) packed
    Just gateway -> fst (gatewayComponentDistance gateway packed cid banked)

candidateDistance :: Vector.Vector (Int, Int) -> Int -> Int
{-# INLINE candidateDistance #-}
candidateDistance candidates packed =
  Vector.foldl' (\best (seed, cost) -> min best (addCostDefault maxBound cost (chebyshevPacked packed seed))) maxBound candidates

componentDistanceCounted :: Heuristic -> Int -> Int -> Bool -> (# Int#, Int# #)
{-# INLINE componentDistanceCounted #-}
componentDistanceCounted heuristic packed cid banked =
  case heuristicGatewayData heuristic of
    Just gateway -> case gatewayComponentDistance gateway packed cid banked of
      (I# distance#, I# count#) -> (# distance#, count# #)
    Nothing -> case (scanGenerators candidates (packed .&. 0x7fff) ((packed `shiftR` 15) .&. 0x7fff), generatorScanLength candidates) of
      (I# distance#, I# count#) -> (# distance#, count# #)
 where
  candidates = heuristicGeneratorScans heuristic Boxed.! seedKey cid banked

gatewayComponentDistance :: GatewayHeuristic -> Int -> Int -> Bool -> (Int, Int)
{-# INLINE gatewayComponentDistance #-}
gatewayComponentDistance gateway packed cid banked =
  let (distance, count) = foldGatewayAttachments network cid (Tile packed) (direct, directCount) addGateway
   in (if distance == maxBound then maxBound else distance `div` 2, count)
 where
  network = gatewayNetwork gateway
  direct
    | Vector.elem cid (gatewayTargetComponents gateway) = twice (chebyshevPacked packed (gatewayTargetPacked gateway))
    | otherwise = maxBound
  directCount = if direct == maxBound then 0 else 1
  addGateway (best, count) sparseVertex attachment =
    (min best (addCostDefault maxBound attachment label), count + 1)
   where
    reverseVertex
      | sparseVertex < gatewayStaticCount gateway = sparseVertex
      | otherwise = gatewayReverseNodeCount gateway + sparseVertex - gatewayStaticCount gateway
    label = gatewayDistances gateway Vector.! stateId reverseVertex banked
  twice value
    | value == maxBound || value > maxBound `div` 2 = maxBound
    | otherwise = value * 2

finite :: Int -> Maybe Int
{-# INLINE finite #-}
finite value
  | value == maxBound = Nothing
  | otherwise = Just value

heuristicFromDistances :: NaturalComponents -> SiteGraph -> TargetOverlay -> Vector.Vector Int -> Double -> Double -> TileReverseCounters -> Heuristic
heuristicFromDistances components graph overlay distances reverseMs seedMs counters =
  heuristicFromSeedTable (seedTableFromDistances components graph overlay distances) graph overlay distances reverseMs seedMs counters

heuristicFromSeedTable :: Boxed.Vector (Vector.Vector (Int, Int)) -> SiteGraph -> TargetOverlay -> Vector.Vector Int -> Double -> Double -> TileReverseCounters -> Heuristic
heuristicFromSeedTable seeds graph overlay distances reverseMs seedMs counters =
  heuristicFromSeedTables seeds (Boxed.map (const Vector.empty) seeds) graph overlay distances reverseMs seedMs counters

heuristicFromSeedTables :: Boxed.Vector (Vector.Vector (Int, Int)) -> Boxed.Vector (Vector.Vector (Int, Int)) -> SiteGraph -> TargetOverlay -> Vector.Vector Int -> Double -> Double -> TileReverseCounters -> Heuristic
heuristicFromSeedTables seeds generators graph overlay distances reverseMs seedMs counters =
  Heuristic seeds generators scans siteIndex
    (Vector.generate (reverseNodeCount * 2) (distances Vector.!))
    Nothing
    reverseMs seedMs counters seedTotal componentCount seedMax seedP50 seedP90 seedP95 seedP99
    generatorTotal generatorMax generatorP50 generatorP90 generatorP95 generatorP99
    ratioP50 ratioP90 ratioP95 ratioP99 ratioMax
 where
  siteIndex
    | targetSynthetic overlay = IntMap.insert (targetPacked overlay) (targetSite overlay) (siteTileIndex graph)
    | otherwise = siteTileIndex graph
  reverseNodeCount = routingNodeCount graph + if targetSynthetic overlay then 1 else 0
  scans = Boxed.zipWith prepare seeds generators
  prepare seedEntries generatorEntries = generatorScanFromVector
    (if Vector.null generatorEntries then seedEntries else generatorEntries)
  (seedTotal, componentCount, seedMax, seedP50, seedP90, seedP95, seedP99) = tableStats seeds
  (generatorTotal, _, generatorMax, generatorP50, generatorP90, generatorP95, generatorP99) = tableStats generators
  (ratioP50, ratioP90, ratioP95, ratioP99, ratioMax) = generatorRatioStats seeds generators

heuristicFromGatewayDistances :: NaturalComponents -> SiteGraph -> TargetOverlay -> ManhattanReverseResult
  -> Double -> TileReverseCounters -> Heuristic
heuristicFromGatewayDistances components graph overlay result reverseMs counters =
  base {heuristicGatewayData = Just gateway}
 where
  doubled = manhattanDistances result
  table = emptyGeneratorTable components
  base = heuristicFromSeedTables table table graph overlay (halveDistances doubled) reverseMs 0 counters
  gateway = GatewayHeuristic
    (siteSparseNetwork graph)
    doubled
    (routingNodeCount graph + if targetSynthetic overlay then 1 else 0)
    (siteStaticCount graph)
    (targetPacked overlay)
    (targetComponents overlay)

tableStats :: Boxed.Vector (Vector.Vector (Int, Int)) -> (Int, Int, Int, Int, Int, Int, Int)
tableStats table = (sum counts, length counts, percentile 100 counts, percentile 50 counts,
  percentile 90 counts, percentile 95 counts, percentile 99 counts)
 where
  counts = sort
    [ Vector.length (table Boxed.! (cid * 2)) + Vector.length (table Boxed.! (cid * 2 + 1))
    | cid <- [0 .. Boxed.length table `div` 2 - 1]
    , not (Vector.null (table Boxed.! (cid * 2)) && Vector.null (table Boxed.! (cid * 2 + 1)))
    ]
  percentile _ [] = 0
  percentile p xs = xs !! ((p * length xs + 99) `div` 100 - 1)

generatorRatioStats :: Boxed.Vector (Vector.Vector (Int, Int)) -> Boxed.Vector (Vector.Vector (Int, Int))
  -> (Double, Double, Double, Double, Double)
generatorRatioStats seeds generators =
  (percentile 50, percentile 90, percentile 95, percentile 99, percentile 100)
 where
  ratios = sort
    [ fromIntegral generatorCount / fromIntegral seedCount
    | cid <- [0 .. Boxed.length seeds `div` 2 - 1]
    , let seedCount = componentCount seeds cid
    , seedCount > 0
    , let generatorCount = componentCount generators cid
    ]
  componentCount table cid =
    Vector.length (table Boxed.! (cid * 2)) + Vector.length (table Boxed.! (cid * 2 + 1))
  percentile _ | null ratios = 0
  percentile p = ratios !! ((p * length ratios + 99) `div` 100 - 1)

emptyGeneratorTable :: NaturalComponents -> Boxed.Vector (Vector.Vector (Int, Int))
emptyGeneratorTable components = Boxed.replicate ((maxComponentId components + 1) * 2) Vector.empty

seedTablesFromManhattanResult :: NaturalComponents -> SiteGraph -> TargetOverlay -> Vector.Vector Int -> ManhattanReverseResult
  -> (Boxed.Vector (Vector.Vector (Int, Int)), Boxed.Vector (Vector.Vector (Int, Int)))
seedTablesFromManhattanResult components graph overlay distances result = runST $ do
  rawTable <- BoxedMutable.replicate tableSize []
  generatorIds <- BoxedMutable.replicate tableSize []
  Vector.iforM_ (siteTiles graph) $ \node packed ->
    let cids = siteComponents graph Boxed.! node in do
      addCandidate rawTable generatorIds cids False node packed
      addCandidate rawTable generatorIds cids True node packed
  when (targetSynthetic overlay) $ do
    addCandidate rawTable generatorIds (targetComponents overlay) False (targetSite overlay) (targetPacked overlay)
    addCandidate rawTable generatorIds (targetComponents overlay) True (targetSite overlay) (targetPacked overlay)
  raw <- Boxed.map Vector.fromList <$> Boxed.freeze rawTable
  ids <- Boxed.freeze generatorIds
  let generators = Boxed.map (Vector.fromList . map generator . uniqueSorted . sort) ids
  pure (raw, generators)
 where
  tableSize = (maxComponentId components + 1) * 2
  generator candidateState =
    ( packedAt (candidateState `div` 2)
    , distances Vector.! candidateState
    )
  addCandidate :: BoxedMutable.MVector s [(Int, Int)] -> BoxedMutable.MVector s [Int]
    -> Vector.Vector Int -> Bool -> Int -> Int -> ST s ()
  addCandidate rawTable generatorIds cids banked node packed = do
    let state = stateId node banked
        distance = distances Vector.! state
    when (distance /= maxBound) $ do
      let originState = manhattanGeneratorOrigins result Vector.! state
          originWeight = manhattanGeneratorWeights result Vector.! state
          originNode = originState `div` 2
          originValid
            | originState < 0 || not (spatialOrTarget originNode) = False
            | odd originState /= banked = False
            | otherwise =
                let originTile = packedAt originNode
                    directDistance = chebyshevPacked originTile packed
                 in directDistance /= maxBound
                    && manhattanDistances result Vector.! state == addCostDefault maxBound originWeight (directDistance * 2)
      Vector.forM_ cids $ \cid -> do
        let key = seedKey cid banked
        seeds <- BoxedMutable.read rawTable key
        BoxedMutable.write rawTable key ((packed, distance) : seeds)
        ids <- BoxedMutable.read generatorIds key
        BoxedMutable.write generatorIds key (validatedOrigin originValid originNode originState cid state : ids)
  validatedOrigin originValid originNode originState cid fallback
    -- A sparse/query walking path is removable only when it is also a direct
    -- Chebyshev geodesic in this component. Shared multi-attachment sites and
    -- any other non-geodesic path conservatively remain their own raw seed.
    | originValid && Vector.elem cid (componentsAt originNode) = originState
    | otherwise = fallback
  spatialCount = Vector.length (siteTiles graph)
  spatialOrTarget node = node < spatialCount || targetSynthetic overlay && node == targetSite overlay
  packedAt node
    | targetSynthetic overlay && node == targetSite overlay = targetPacked overlay
    | otherwise = siteTiles graph Vector.! node
  componentsAt node
    | targetSynthetic overlay && node == targetSite overlay = targetComponents overlay
    | otherwise = siteComponents graph Boxed.! node
  uniqueSorted [] = []
  uniqueSorted (x:xs) = x : go x xs
   where
    go _ [] = []
    go previous (value:rest)
      | value == previous = go previous rest
      | otherwise = value : go value rest

seedTableFromDistances :: NaturalComponents -> SiteGraph -> TargetOverlay -> Vector.Vector Int -> Boxed.Vector (Vector.Vector (Int, Int))
seedTableFromDistances components graph overlay distances = runST $ do
  table <- Boxed.thaw (Boxed.replicate ((maxComponentId components + 1) * 2) [])
  Vector.iforM_ (siteTiles graph) $ \node packed ->
    Vector.forM_ (siteComponents graph Boxed.! node) $ \cid -> do
      addSeed table cid False packed (distances Vector.! stateId node False)
      addSeed table cid True packed (distances Vector.! stateId node True)
  when (targetSynthetic overlay) $
    Vector.forM_ (targetComponents overlay) $ \cid -> do
      addSeed table cid False (targetPacked overlay) (distances Vector.! stateId (targetSite overlay) False)
      addSeed table cid True (targetPacked overlay) (distances Vector.! stateId (targetSite overlay) True)
  Boxed.map Vector.fromList <$> Boxed.freeze table
 where
  addSeed table cid banked packed distance =
    when (distance /= maxBound) $ do
      seeds <- BoxedMutable.read table (seedKey cid banked)
      BoxedMutable.write table (seedKey cid banked) ((packed, distance) : seeds)

seedKey :: Int -> Bool -> Int
seedKey cid banked = cid * 2 + if banked then 1 else 0

forceReverseResult :: (Vector.Vector Int, TileReverseCounters, Maybe ManhattanReverseResult) -> IO Int
forceReverseResult (distances, counters, provenance) =
  evaluate
    ( Vector.sum distances
        + case provenance of
            Nothing -> 0
            Just result -> Vector.sum (manhattanGeneratorOrigins result) + Vector.sum (manhattanGeneratorWeights result)
        + reverseStatesPopped counters
        + reverseStalePqEntries counters
        + reversePqPushes counters
        + reversePqPops counters
        + reversePqMaxSize counters
        + reverseEdgesRelaxed counters
        + reverseSameComponentSiteScans counters
        + reverseTotalSitesScanned counters
        + reverseChebyshevComparisons counters
        + reverseMaxSitesScannedPerPop counters
        + reverseTransportRelaxations counters
    )

forceGatewayReverseResult :: (ManhattanReverseResult, TileReverseCounters) -> IO Int
forceGatewayReverseResult (result, counters) = evaluate
  ( Vector.sum (manhattanDistances result)
      + reverseStatesPopped counters
      + reverseEdgesRelaxed counters
      + reverseTransportRelaxations counters
  )

forceSeedTables :: (Boxed.Vector (Vector.Vector (Int, Int)), Boxed.Vector (Vector.Vector (Int, Int))) -> IO Int
forceSeedTables (seeds, generators) = evaluate (forceTable seeds + forceTable generators)
 where
  forceTable = Boxed.ifoldl' (\total ix entries -> total + ix + Vector.length entries) 0
