{-# LANGUAGE MonoLocalBinds #-}

module ShortestPath.Exact.TileAStar
  ( TileAStar
  , TileAStarCounters(..)
  , TileBankGlobalObservation(..)
  , TileReverseCounters(..)
  , TileAStarTimings(..)
  , TileAStarConfig(..)
  , CompiledRoutingAccount
  , compiledRoutingFingerprint
  , EffectiveRoutingFingerprint
  , PreparedTarget
  , preparedTargetTile
  , RoutingOptions(..)
  , SearchOptions(..)
  , ReverseImplementation(..)
  , defaultTileAStarConfig
  , buildTileAStar
  , buildTileAStarWithPolicy
  , buildTileAStarFromTopology
  , tileTopology
  , compileRoutingAccount
  , compileRoutingAccountProfiled
  , routingOptionsFromQuery
  , searchOptionsFromQuery
  , prepareTarget
  , prepareTargetProfiled
  , searchPrepared
  , searchPreparedProfiled
  , searchPreparedProfiledWithTrace
  , recordPreparationTimings
  , findRouteTileAStar
  , findRouteProfiledTileAStar
  , findRouteProfiledTileAStarWithConfig
  , findRouteProfiledTileAStarWithTrace
  , findRouteProfiledTileAStarWithTraceConfig
  , forceTileAStar
  , tileStaticStats
  ) where

import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import GHC.Clock (getMonotonicTimeNSec)
import System.Mem (getAllocationCounter)

import ShortestPath.Pathfinder
import ShortestPath.Exact.TileAStar.Heuristic
import ShortestPath.Exact.TileAStar.HeuristicScan (forceGeneratorScan)
import ShortestPath.Exact.TileAStar.Preprocessing
import ShortestPath.Exact.TileAStar.RelaxedGraph (binarySearch, compileRoutingAccount)
import ShortestPath.Exact.TileAStar.ReverseSearch (emptyReverseCounters)
import ShortestPath.Exact.TileAStar.Search
import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.Timing
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.World

findRouteTileAStar :: TileAStar -> Query -> Route
findRouteTileAStar astar q =
  searchPrepared astar account target (queryStart q) (searchOptionsFromQuery q)
 where
  account = compileRoutingAccount astar (routingOptionsFromQuery q)
  target = prepareTarget astar account (queryTarget q)

prepareTarget :: TileAStar -> CompiledRoutingAccount -> Tile -> PreparedTarget
prepareTarget astar account target = preparedTarget astar target (prepareHeuristic astar account target)

compileRoutingAccountProfiled :: TileAStar -> RoutingOptions -> IO (CompiledRoutingAccount, Double)
compileRoutingAccountProfiled astar options =
  timedIO forceCompiledRoutingAccount (pure (compileRoutingAccount astar options))

prepareTargetProfiled :: TileAStarConfig -> TileAStar -> CompiledRoutingAccount -> Tile -> IO (PreparedTarget, Double)
prepareTargetProfiled config astar account target =
  timedIO forcePreparedTarget
    (preparedTarget astar target <$> prepareHeuristicProfiled config astar account target)

preparedTarget :: TileAStar -> Tile -> Heuristic -> PreparedTarget
preparedTarget (TileAStar topology static) target heuristic =
  PreparedTarget target attachments site extras extraComponents extraSites heuristic
 where
  attachments = Vector.fromList (structurallyReachablePointAttachments topology target)
  site = IntMap.findWithDefault (-1) (unTile target) (heuristicSiteIndex heuristic)
  extras = Vector.fromList
    [ packed
    | packed <- IntSet.toAscList (IntSet.insert (unTile target) (IntSet.fromList (Vector.toList (staticTiles static))))
    , binarySearch packed (staticSearchTiles static) == Nothing
    ]
  staticIndex = IntMap.fromList [(packed, ix) | (ix, packed) <- Vector.toList (Vector.indexed (staticTiles static))]
  extraComponents = Boxed.fromList [componentsAt packed | packed <- Vector.toList extras]
  componentsAt packed
    | packed == unTile target = attachments
    | otherwise = maybe Vector.empty (staticComponents static Boxed.!) (IntMap.lookup packed staticIndex)
  extraSites = Vector.map (\packed -> IntMap.findWithDefault (-1) packed (heuristicSiteIndex heuristic)) extras

searchPrepared :: TileAStar -> CompiledRoutingAccount -> PreparedTarget -> Tile -> SearchOptions -> Route
searchPrepared astar account target start options =
  let (route, _, _) = search False astar account target start options
   in route

searchPreparedProfiled :: TileAStar -> CompiledRoutingAccount -> PreparedTarget -> Tile -> SearchOptions -> IO (Route, TileAStarTimings)
searchPreparedProfiled astar account target start options = do
  (route, timings, _) <- searchPreparedProfiledWithTrace False astar account target start options
  pure (route, timings)

searchPreparedProfiledWithTrace :: Bool -> TileAStar -> CompiledRoutingAccount -> PreparedTarget -> Tile -> SearchOptions -> IO (Route, TileAStarTimings, [(Tile, Bool)])
searchPreparedProfiledWithTrace trace astar account target start options = do
  beforeAlloc <- getAllocationCounter
  started <- getMonotonicTimeNSec
  let result@(route, counters, explored) = search trace astar account target start options
  _ <- forceSearch result
  finished <- getMonotonicTimeNSec
  afterAlloc <- getAllocationCounter
  let searchMs = milliseconds started finished
  pure
    ( route
    , TileAStarTimings
        { tileAccountPrepareMilliseconds = 0
        , tileTargetPrepareMilliseconds = 0
        , tileHeuristicSetupMilliseconds = 0
        , tileReverseDijkstraMilliseconds = 0
        , tileSeedTableMilliseconds = 0
        , tileHeuristicSeedCount = 0
        , tileHeuristicComponentCount = 0
        , tileHeuristicMaxSeedsPerComponent = 0
        , tileHeuristicSeedsPerComponentP50 = 0
        , tileHeuristicSeedsPerComponentP90 = 0
        , tileHeuristicSeedsPerComponentP95 = 0
        , tileHeuristicSeedsPerComponentP99 = 0
        , tileHeuristicGeneratorCount = 0
        , tileHeuristicMaxGeneratorsPerComponent = 0
        , tileHeuristicGeneratorsPerComponentP50 = 0
        , tileHeuristicGeneratorsPerComponentP90 = 0
        , tileHeuristicGeneratorsPerComponentP95 = 0
        , tileHeuristicGeneratorsPerComponentP99 = 0
        , tileHeuristicGeneratorSeedRatioP50 = 0
        , tileHeuristicGeneratorSeedRatioP90 = 0
        , tileHeuristicGeneratorSeedRatioP95 = 0
        , tileHeuristicGeneratorSeedRatioP99 = 0
        , tileHeuristicGeneratorSeedRatioMax = 0
        , tileHeuristicGeneratorSeedRatio = 0
        , tileSearchMilliseconds = searchMs
        , tileForwardAllocatedBytes = beforeAlloc - afterAlloc
        , tileTotalMilliseconds = searchMs
        , tileSearchCounters = counters
        , tileReverseCounters = emptyReverseCounters
        }
    , explored
    )

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
        + Vector.length (staticNorthNodes static)
        + Vector.length (staticSouthNodes static)
        + Vector.length (staticTiles static)
        + IntMap.size (staticSiteTileIndex static)
        + Boxed.foldl' (\total sites -> total + Vector.length sites) 0 (staticSiteComponentIds static)
        + Set.size (staticReachableBanks static)
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
    (account, accountMs) <- compileRoutingAccountProfiled astar (routingOptionsFromQuery query)
    (target, targetMs) <- prepareTargetProfiled config astar account (queryTarget query)
    (route, timings, explored) <- searchPreparedProfiledWithTrace trace astar account target (queryStart query) (searchOptionsFromQuery query)
    pure (route, recordPreparationTimings accountMs targetMs target timings, explored)

recordPreparationTimings :: Double -> Double -> PreparedTarget -> TileAStarTimings -> TileAStarTimings
recordPreparationTimings accountMs targetMs target timings = timings
  { tileAccountPrepareMilliseconds = accountMs
  , tileTargetPrepareMilliseconds = targetMs
  , tileHeuristicSetupMilliseconds = accountMs + targetMs
  , tileReverseDijkstraMilliseconds = if targetMs == 0 then 0 else heuristicReverseMilliseconds heuristic
  , tileSeedTableMilliseconds = if targetMs == 0 then 0 else heuristicSeedTableMilliseconds heuristic
  , tileHeuristicSeedCount = heuristicSeedCount heuristic
  , tileHeuristicComponentCount = heuristicComponentCount heuristic
  , tileHeuristicMaxSeedsPerComponent = heuristicMaxSeedsPerComponent heuristic
  , tileHeuristicSeedsPerComponentP50 = heuristicSeedsPerComponentP50 heuristic
  , tileHeuristicSeedsPerComponentP90 = heuristicSeedsPerComponentP90 heuristic
  , tileHeuristicSeedsPerComponentP95 = heuristicSeedsPerComponentP95 heuristic
  , tileHeuristicSeedsPerComponentP99 = heuristicSeedsPerComponentP99 heuristic
  , tileHeuristicGeneratorCount = heuristicGeneratorCount heuristic
  , tileHeuristicMaxGeneratorsPerComponent = heuristicMaxGeneratorsPerComponent heuristic
  , tileHeuristicGeneratorsPerComponentP50 = heuristicGeneratorsPerComponentP50 heuristic
  , tileHeuristicGeneratorsPerComponentP90 = heuristicGeneratorsPerComponentP90 heuristic
  , tileHeuristicGeneratorsPerComponentP95 = heuristicGeneratorsPerComponentP95 heuristic
  , tileHeuristicGeneratorsPerComponentP99 = heuristicGeneratorsPerComponentP99 heuristic
  , tileHeuristicGeneratorSeedRatioP50 = heuristicGeneratorSeedRatioP50 heuristic
  , tileHeuristicGeneratorSeedRatioP90 = heuristicGeneratorSeedRatioP90 heuristic
  , tileHeuristicGeneratorSeedRatioP95 = heuristicGeneratorSeedRatioP95 heuristic
  , tileHeuristicGeneratorSeedRatioP99 = heuristicGeneratorSeedRatioP99 heuristic
  , tileHeuristicGeneratorSeedRatioMax = heuristicGeneratorSeedRatioMax heuristic
  , tileHeuristicGeneratorSeedRatio = if heuristicSeedCount heuristic == 0 then 0 else
      fromIntegral (heuristicGeneratorCount heuristic) / fromIntegral (heuristicSeedCount heuristic)
  , tileTotalMilliseconds = accountMs + targetMs + tileSearchMilliseconds timings
  , tileReverseCounters = if targetMs == 0 then emptyReverseCounters else heuristicReverseCounters heuristic
  }
 where
  heuristic = preparedTargetHeuristic target

forceCompiledRoutingAccount :: CompiledRoutingAccount -> IO Int
forceCompiledRoutingAccount account = evaluate
  ( Map.foldl' (\total transports -> total + length transports) 0 (carriedLocalTransports availability)
      + Map.foldl' (\total transports -> total + length transports) 0 (bankedLocalTransports availability)
      + length (carriedGlobalTransports availability)
      + length (bankedGlobalTransports availability)
      + Map.size (compiledTransportPenalties account)
      + Vector.length (siteTiles graph)
      + IntMap.size (siteTileIndex graph)
      + Boxed.foldl' (\total components -> total + Vector.length components) 0 (siteComponents graph)
      + Boxed.foldl' (\total sites -> total + Vector.length sites) 0 (siteComponentSiteIds graph)
      + Boxed.foldl' (\total edges -> total + Vector.length edges) 0 (siteReverseEdges graph)
  )
 where
  availability = compiledTransportAvailability account
  graph = compiledSiteGraph account

forceHeuristic :: Heuristic -> IO Int
forceHeuristic heuristic =
  evaluate
    ( Boxed.foldl' (\total seeds -> total + Vector.length seeds) 0 (heuristicSeeds heuristic)
        + Boxed.foldl' (\total scan -> total + forceGeneratorScan scan) 0 (heuristicGeneratorScans heuristic)
        + round (heuristicReverseMilliseconds heuristic)
        + round (heuristicSeedTableMilliseconds heuristic)
        + reverseStatesPopped (heuristicReverseCounters heuristic)
        + heuristicSeedCount heuristic
        + heuristicComponentCount heuristic
        + heuristicMaxSeedsPerComponent heuristic
        + heuristicSeedsPerComponentP50 heuristic
        + heuristicSeedsPerComponentP90 heuristic
        + heuristicSeedsPerComponentP95 heuristic
        + heuristicSeedsPerComponentP99 heuristic
        + heuristicGeneratorCount heuristic
        + heuristicMaxGeneratorsPerComponent heuristic
        + heuristicGeneratorsPerComponentP50 heuristic
        + heuristicGeneratorsPerComponentP90 heuristic
        + heuristicGeneratorsPerComponentP95 heuristic
        + heuristicGeneratorsPerComponentP99 heuristic
        + truncate (heuristicGeneratorSeedRatioP50 heuristic)
        + truncate (heuristicGeneratorSeedRatioP90 heuristic)
        + truncate (heuristicGeneratorSeedRatioP95 heuristic)
        + truncate (heuristicGeneratorSeedRatioP99 heuristic)
        + truncate (heuristicGeneratorSeedRatioMax heuristic)
    )

forcePreparedTarget :: PreparedTarget -> IO Int
forcePreparedTarget target = do
  forcedHeuristic <- forceHeuristic (preparedTargetHeuristic target)
  evaluate
    ( forcedHeuristic
        + Vector.length (preparedTargetAttachments target)
        + Vector.length (preparedSearchExtraTiles target)
        + Boxed.foldl' (\total components -> total + Vector.length components) 0 (preparedSearchExtraComponents target)
        + Vector.sum (preparedSearchExtraSites target)
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
        + tileHeuristicCalls counters
        + tileHeuristicCandidatesScanned counters
        + tileHeuristicMaxCandidatesPerCall counters
        + tileHeuristicUnreachable counters
        + tileUnknownComponentPrunes counters
        + tileNoReverseSeedPrunes counters
        + tileBestBankCostUpdates counters
        + tileFinalBestBankCost counters
        + tileBankDominatedHeuristicEvaluations counters
        + tileBankGlobalTransitionsSuppressed counters
        + tileBankBoundPQRekeys counters
    )
