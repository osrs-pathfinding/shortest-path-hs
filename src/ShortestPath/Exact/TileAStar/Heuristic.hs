module ShortestPath.Exact.TileAStar.Heuristic
  ( Heuristic(..)
  , ReverseImplementation(..)
  , TileAStarConfig(..)
  , defaultTileAStarConfig
  , heuristicAt
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
defaultTileAStarConfig = TileAStarConfig CliqueReverse False False

data Heuristic = Heuristic
  { heuristicSeeds :: Boxed.Vector (Vector.Vector (Int, Int))
  , heuristicSiteIndex :: IntMap.IntMap Int
  , heuristicSiteDistances :: Vector.Vector Int
  , heuristicReverseMilliseconds :: !Double
  , heuristicSeedTableMilliseconds :: !Double
  , heuristicReverseCounters :: !TileReverseCounters
  }

-- | The production, untimed heuristic used by the pure RouteFinder API.
prepareHeuristic :: TileAStar -> Query -> QueryTransportAvailability -> Heuristic
prepareHeuristic astar q availability =
  heuristicFromDistances components graph distances 0 0 emptyReverseCounters
 where
  graph = siteGraph astar q availability
  distances = reverseDijkstraUncounted graph (targetSeeds graph (queryTarget q))
  components = topologyNaturalComponents (tileTopology astar)

prepareHeuristicProfiled :: TileAStarConfig -> TileAStar -> Query -> QueryTransportAvailability -> IO Heuristic
prepareHeuristicProfiled config astar q availability = do
  ((distances, counters), reverseMs) <- timedIO forceReverseResult reverseAction
  (table, seedMs) <- timedIO forceSeedTable (pure (seedTableFromDistances components graph distances))
  pure (heuristicFromSeedTable table graph distances reverseMs seedMs counters)
 where
  graph = siteGraph astar q availability
  seeds = targetSeeds graph (queryTarget q)
  components = topologyNaturalComponents (tileTopology astar)
  reverseAction = case tileReverseImplementation config of
    CliqueReverse
      | tileCollectReverseCounters config -> pure (reverseDijkstra graph seeds)
      | otherwise -> pure (reverseDijkstraUncounted graph seeds, emptyReverseCounters)
    SparseWalkingReverse -> do
      let distances = halveDistances (reverseDijkstraManhattanUncounted graph seeds)
      when (tileCompareReverseImplementations config)
        (assertReverseLabelsEqual graph (reverseDijkstraUncounted graph seeds) distances)
      pure (distances, emptyReverseCounters)

heuristicAt :: WorldTopology -> Heuristic -> Tile -> Bool -> Maybe Int
heuristicAt topology heuristic tile banked =
  minimumMaybe (exactSiteDistance : map componentDistance (structurallyReachablePointAttachments topology tile))
 where
  exactSiteDistance = do
    node <- IntMap.lookup (unTile tile) (heuristicSiteIndex heuristic)
    distance <- heuristicSiteDistances heuristic Vector.!? stateId node banked
    if distance == maxBound then Nothing else Just distance
  seedDistance (packed, cost) = addCostDefault maxBound cost (chebyshevPacked (unTile tile) packed)
  componentDistance cid =
    let seeds = heuristicSeeds heuristic Boxed.! seedKey cid banked
     in if Vector.null seeds then Nothing else Just (Vector.minimum (Vector.map seedDistance seeds))
  minimumMaybe values = case [value | Just value <- values] of
    [] -> Nothing
    finite -> Just (minimum finite)

heuristicFromDistances :: NaturalComponents -> SiteGraph -> Vector.Vector Int -> Double -> Double -> TileReverseCounters -> Heuristic
heuristicFromDistances components graph distances reverseMs seedMs counters =
  heuristicFromSeedTable (seedTableFromDistances components graph distances) graph distances reverseMs seedMs counters

heuristicFromSeedTable :: Boxed.Vector (Vector.Vector (Int, Int)) -> SiteGraph -> Vector.Vector Int -> Double -> Double -> TileReverseCounters -> Heuristic
heuristicFromSeedTable seeds graph distances reverseMs seedMs counters =
  Heuristic seeds (siteTileIndex graph)
    (Vector.generate (Vector.length (siteTiles graph) * 2) (distances Vector.!))
    reverseMs seedMs counters

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
        + reverseSameComponentSiteScans counters
        + reverseTotalSitesScanned counters
        + reverseChebyshevComparisons counters
        + reverseMaxSitesScannedPerPop counters
        + reverseTransportRelaxations counters
    )

forceSeedTable :: Boxed.Vector (Vector.Vector (Int, Int)) -> IO Int
forceSeedTable table = evaluate (Boxed.ifoldl' (\total ix seeds -> total + ix + Vector.length seeds) 0 table)
