{-# LANGUAGE MonoLocalBinds #-}

module ShortestPath.Exact.TileAStar
  ( TileAStar
  , TileAStarCounters(..)
  , TileBankGlobalObservation(..)
  , TileReverseCounters(..)
  , TileAStarTimings(..)
  , TileAStarConfig(..)
  , ReverseImplementation(..)
  , defaultTileAStarConfig
  , buildTileAStar
  , buildTileAStarWithPolicy
  , buildTileAStarFromTopology
  , tileTopology
  , findRouteTileAStar
  , findRouteProfiledTileAStar
  , findRouteProfiledTileAStarWithConfig
  , findRouteProfiledTileAStarWithTrace
  , findRouteProfiledTileAStarWithTraceConfig
  , forceTileAStar
  , tileStaticStats
  ) where

import Control.Exception (evaluate)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import GHC.Clock (getMonotonicTimeNSec)
import System.Mem (getAllocationCounter)

import ShortestPath.Pathfinder
import ShortestPath.Exact.TileAStar.Heuristic
import ShortestPath.Exact.TileAStar.Preprocessing
import ShortestPath.Exact.TileAStar.Search
import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.Timing
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.World

findRouteTileAStar :: TileAStar -> Query -> Route
findRouteTileAStar astar q =
  let availability = prepareQueryTransports (tileWorld astar) q
      (route, _, _) = search False astar q availability (prepareHeuristic astar q availability)
   in route

buildTileAStar :: World -> IO TileAStar
buildTileAStar world = buildWorldTopology world >>= buildTileAStarFromTopology

buildTileAStarWithPolicy :: StructuralReachabilityPolicy -> World -> IO (Either ReachabilityError TileAStar)
buildTileAStarWithPolicy policy world = do
  topology <- buildWorldTopologyWithPolicy policy world
  traverse buildTileAStarFromTopology topology

buildTileAStarFromTopology :: WorldTopology -> IO TileAStar
buildTileAStarFromTopology topology =
  pure (TileAStar topology (buildTileStatic topology))

forceTileAStar :: TileAStar -> IO TileAStar
forceTileAStar astar@(TileAStar topology static) = do
  _ <- evaluate
    ( Vector.length (componentOwnerTiles components)
        + Vector.length (componentOwnerIds components)
        + Vector.length (componentIds components)
        + maxComponentId components
        + Vector.length (staticSearchTiles static)
        + Vector.length (staticSearchComponents static)
        + Vector.length (staticWalkingMasks static)
        + Vector.length (staticTiles static)
        + sparseVertexCount (staticWalkingNetwork static)
        + sparseWalkingEdgeCount (staticWalkingNetwork static)
    )
  pure astar
 where
  components = topologyNaturalComponents topology

findRouteProfiledTileAStar :: TileAStar -> Query -> IO (Route, TileAStarTimings)
findRouteProfiledTileAStar = findRouteProfiledTileAStarWithConfig defaultTileAStarConfig

findRouteProfiledTileAStarWithConfig :: TileAStarConfig -> TileAStar -> Query -> IO (Route, TileAStarTimings)
findRouteProfiledTileAStarWithConfig config astar query = do
  (route, timings, _) <- findRouteProfiledTileAStarWithTraceConfig config False astar query
  pure (route, timings)

findRouteProfiledTileAStarWithTrace :: Bool -> TileAStar -> Query -> IO (Route, TileAStarTimings, [(Tile, Bool)])
findRouteProfiledTileAStarWithTrace = findRouteProfiledTileAStarWithTraceConfig defaultTileAStarConfig

findRouteProfiledTileAStarWithTraceConfig :: TileAStarConfig -> Bool -> TileAStar -> Query -> IO (Route, TileAStarTimings, [(Tile, Bool)])
findRouteProfiledTileAStarWithTraceConfig config trace astar query =
  do
    let availability = prepareQueryTransports (tileWorld astar) query
    (heuristic, setupMs) <- timedIO forceHeuristic (prepareHeuristicProfiled config astar query availability)
    beforeAlloc <- getAllocationCounter
    started <- getMonotonicTimeNSec
    let result@(route, counters, explored) = search trace astar query availability heuristic
    _ <- forceSearch result
    finished <- getMonotonicTimeNSec
    afterAlloc <- getAllocationCounter
    let searchMs = milliseconds started finished
        allocatedBytes = beforeAlloc - afterAlloc
    let totalMs = setupMs + searchMs
    pure
      ( route
      , TileAStarTimings
          setupMs
          (heuristicReverseMilliseconds heuristic)
          (heuristicSeedTableMilliseconds heuristic)
          searchMs
          allocatedBytes
          totalMs
          counters
          (heuristicReverseCounters heuristic)
      , explored
      )

forceHeuristic :: Heuristic -> IO Int
forceHeuristic heuristic =
  evaluate
    ( Boxed.foldl' (\total seeds -> total + Vector.length seeds) 0 (heuristicSeeds heuristic)
        + round (heuristicReverseMilliseconds heuristic)
        + round (heuristicSeedTableMilliseconds heuristic)
        + reverseStatesPopped (heuristicReverseCounters heuristic)
    )

forceSearch :: (Route, TileAStarCounters, [(Tile, Bool)]) -> IO Int
forceSearch (route, counters, explored) =
  evaluate
    ( routeCost route
        + routeExpandedNodes route
        + length (routeSteps route)
        + length explored
        + tileStatesPopped counters
        + tileStalePqEntries counters
        + tilePqPushes counters
        + tileUniqueStatesReached counters
        + tileWalkingRelaxations counters
        + tileTransportRelaxations counters
        + tileHeuristicEvaluations counters
        + tileHeuristicUnreachable counters
        + tileUnknownComponentPrunes counters
        + tileNoReverseSeedPrunes counters
        + tileBestBankCostUpdates counters
        + tileFinalBestBankCost counters
        + tileBankDominatedHeuristicEvaluations counters
        + tileBankGlobalTransitionsSuppressed counters
        + tileBankBoundPQRekeys counters
    )
