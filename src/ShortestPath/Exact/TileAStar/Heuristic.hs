{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ShortestPath.Exact.TileAStar.Heuristic
  ( Heuristic(..)
  , PreparedTarget(..)
  , ReverseImplementation(..)
  , TileAStarConfig(..)
  , defaultTileAStarConfig
  , heuristicAt
  , heuristicAtComponent
  , heuristicAtComponentRaw
  , heuristicAtComponentCountedRaw
  , heuristicAtResolved
  , heuristicAtResolvedRaw
  , heuristicAtResolvedCountedRaw
  , heuristicFromDistances
  , prepareHeuristic
  , prepareHeuristicProfiled
  , seedKey
  ) where

import Control.Exception (evaluate)
import Control.Monad (when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Unboxed as Vector
import Control.Monad.ST (runST)
import Data.List (sort)
import GHC.Exts (Int(I#), Int#)

import ShortestPath.Exact.TileAStar.RelaxedGraph
import ShortestPath.Exact.TileAStar.ReverseSearch
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.Timing
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology

data ReverseImplementation = CliqueReverse | SparseWalkingReverse
  deriving stock (Eq, Show)

data TileAStarConfig = TileAStarConfig
  { tileReverseImplementation :: !ReverseImplementation
  , tileCompareReverseImplementations :: !Bool
  , tileCollectReverseCounters :: !Bool
  }
  deriving stock (Eq, Show)

defaultTileAStarConfig :: TileAStarConfig
defaultTileAStarConfig = TileAStarConfig CliqueReverse False True

data Heuristic = Heuristic
  { heuristicSeeds :: Boxed.Vector (Vector.Vector (Int, Int))
  , heuristicSiteIndex :: IntMap.IntMap Int
  , heuristicSiteDistances :: Vector.Vector Int
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
  }

data PreparedTarget = PreparedTarget
  { preparedTargetTile :: !Tile
  , preparedTargetAttachments :: Vector.Vector Int
  , preparedTargetSite :: !Int
  , preparedSearchExtraTiles :: Vector.Vector Int
  , preparedSearchExtraComponents :: Boxed.Vector (Vector.Vector Int)
  , preparedSearchExtraSites :: Vector.Vector Int
  , preparedTargetHeuristic :: Heuristic
  }

-- | The production, untimed heuristic used by the pure Tile A* entry point.
prepareHeuristic :: TileAStar -> CompiledRoutingAccount -> Tile -> Heuristic
prepareHeuristic astar account target =
  heuristicFromDistances components graph distances 0 0 emptyReverseCounters
 where
  graph = siteGraph astar account target
  distances = reverseDijkstraUncounted graph (targetSeeds graph target)
  components = topologyNaturalComponents (tileTopology astar)

prepareHeuristicProfiled :: TileAStarConfig -> TileAStar -> CompiledRoutingAccount -> Tile -> IO Heuristic
prepareHeuristicProfiled config astar account target = do
  ((distances, counters), reverseMs) <- timedIO forceReverseResult reverseAction
  (table, seedMs) <- timedIO forceSeedTable (pure (seedTableFromDistances components graph distances))
  pure (heuristicFromSeedTable table graph distances reverseMs seedMs counters)
 where
  graph = siteGraph astar account target
  seeds = targetSeeds graph target
  components = topologyNaturalComponents (tileTopology astar)
  reverseAction = case tileReverseImplementation config of
    CliqueReverse
      | tileCollectReverseCounters config -> pure (reverseDijkstra graph seeds)
      | otherwise -> pure (reverseDijkstraUncounted graph seeds, emptyReverseCounters)
    SparseWalkingReverse -> do
      let (rawDistances, counters)
            | tileCollectReverseCounters config = reverseDijkstraManhattan graph seeds
            | otherwise = (reverseDijkstraManhattanUncounted graph seeds, emptyReverseCounters)
          distances = halveDistances rawDistances
      when (tileCompareReverseImplementations config)
        (assertReverseLabelsEqual graph (reverseDijkstraUncounted graph seeds) distances)
      pure (distances, counters)

heuristicAt :: WorldTopology -> Heuristic -> Tile -> Bool -> Maybe Int
{-# INLINE heuristicAt #-}
heuristicAt topology heuristic tile banked =
  heuristicAtResolved heuristic packed attachments site banked
 where
  packed = unTile tile
  attachments = Vector.fromList (structurallyReachablePointAttachments topology tile)
  site = IntMap.findWithDefault (-1) packed (heuristicSiteIndex heuristic)

heuristicAtComponent :: Heuristic -> Int -> Int -> Bool -> Maybe Int
{-# INLINE heuristicAtComponent #-}
heuristicAtComponent heuristic packed cid banked =
  finite (heuristicAtComponentRaw heuristic packed cid banked)

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
  Vector.foldl' (\best (seed, cost) -> min best (addCostDefault maxBound cost (chebyshevPacked packed seed))) maxBound
    (heuristicSeeds heuristic Boxed.! seedKey cid banked)

componentDistanceCounted :: Heuristic -> Int -> Int -> Bool -> (# Int#, Int# #)
{-# INLINE componentDistanceCounted #-}
componentDistanceCounted heuristic packed cid banked =
  case (componentDistance heuristic packed cid banked, Vector.length seeds) of
    (I# distance#, I# count#) -> (# distance#, count# #)
 where
  seeds = heuristicSeeds heuristic Boxed.! seedKey cid banked

finite :: Int -> Maybe Int
{-# INLINE finite #-}
finite value
  | value == maxBound = Nothing
  | otherwise = Just value

heuristicFromDistances :: NaturalComponents -> SiteGraph -> Vector.Vector Int -> Double -> Double -> TileReverseCounters -> Heuristic
heuristicFromDistances components graph distances reverseMs seedMs counters =
  heuristicFromSeedTable (seedTableFromDistances components graph distances) graph distances reverseMs seedMs counters

heuristicFromSeedTable :: Boxed.Vector (Vector.Vector (Int, Int)) -> SiteGraph -> Vector.Vector Int -> Double -> Double -> TileReverseCounters -> Heuristic
heuristicFromSeedTable seeds graph distances reverseMs seedMs counters =
  Heuristic seeds (siteTileIndex graph)
    (Vector.generate (Vector.length (siteTiles graph) * 2) (distances Vector.!))
    reverseMs seedMs counters total componentCount maximumCount p50 p90 p95 p99
 where
  counts = sort
    [ Vector.length (seeds Boxed.! (cid * 2)) + Vector.length (seeds Boxed.! (cid * 2 + 1))
    | cid <- [0 .. Boxed.length seeds `div` 2 - 1]
    , not (Vector.null (seeds Boxed.! (cid * 2)) && Vector.null (seeds Boxed.! (cid * 2 + 1)))
    ]
  total = sum counts
  componentCount = length counts
  maximumCount = percentile 100 counts
  p50 = percentile 50 counts
  p90 = percentile 90 counts
  p95 = percentile 95 counts
  p99 = percentile 99 counts
  percentile _ [] = 0
  percentile p xs = xs !! ((p * length xs + 99) `div` 100 - 1)

seedTableFromDistances :: NaturalComponents -> SiteGraph -> Vector.Vector Int -> Boxed.Vector (Vector.Vector (Int, Int))
seedTableFromDistances components graph distances = runST $ do
  table <- Boxed.thaw (Boxed.replicate ((maxComponentId components + 1) * 2) [])
  Vector.iforM_ (siteTiles graph) $ \node packed ->
    Vector.forM_ (siteComponents graph Boxed.! node) $ \cid -> do
      addSeed table cid False packed (distances Vector.! stateId node False)
      addSeed table cid True packed (distances Vector.! stateId node True)
  Boxed.map Vector.fromList <$> Boxed.freeze table
 where
  addSeed table cid banked packed distance =
    when (distance /= maxBound) $ do
      seeds <- BoxedMutable.read table (seedKey cid banked)
      BoxedMutable.write table (seedKey cid banked) ((packed, distance) : seeds)

seedKey :: Int -> Bool -> Int
seedKey cid banked = cid * 2 + if banked then 1 else 0

forceReverseResult :: (Vector.Vector Int, TileReverseCounters) -> IO Int
forceReverseResult (distances, counters) =
  evaluate
    ( Vector.sum distances
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

forceSeedTable :: Boxed.Vector (Vector.Vector (Int, Int)) -> IO Int
forceSeedTable table = evaluate (Boxed.ifoldl' (\total ix seeds -> total + ix + Vector.length seeds) 0 table)
