{-# LANGUAGE MonoLocalBinds #-}

module ShortestPath.Exact.Hierarchical
  ( Hierarchical(..)
  , QueryTimings(..)
  , SearchCounters(..)
  , findRouteProfiled
  ) where

import Control.Exception (assert, evaluate)
import Control.Monad (foldM, forM)
import Control.Monad.ST (runST)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Min as PQueue
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as BoxedMutable
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Preprocess
import ShortestPath.Hierarchy.Types
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

data Hierarchical = Hierarchical World Hierarchy

data Edge
  = EdgeWalk Int Tile
  | EdgeTransport Int String Tile
  | EdgeMetric Int Tile Tile
  | EdgeAttach Tile
  deriving stock (Eq, Show)

data SearchResult
  = SearchFailed Int SearchCounters
  | SearchFound Int Int (Vector.Vector Int) (Boxed.Vector (Maybe Edge)) Int SearchCounters

data DenseIndex = DenseIndex
  { denseNodes :: Boxed.Vector Node
  , denseTileNodes :: IntMap.IntMap Int
  , denseGlobalHub :: Int
  , denseQuerySource :: Int
  , denseQueryTarget :: Int
  }

data EdgeKind
  = SourceEdge
  | TargetEdge
  | MetricEdge
  | SeparatorEdge
  | LocalTransportEdge
  | GlobalEntryEdge
  | GlobalTeleportEdge
  | BankEdge

data SearchCounters = SearchCounters
  { searchQueuePops :: !Int
  , searchStalePops :: !Int
  , searchEdgesConsidered :: !Int
  , searchSuccessfulRelaxations :: !Int
  , searchSourceEdges :: !Int
  , searchTargetEdges :: !Int
  , searchMetricEdges :: !Int
  , searchSeparatorEdges :: !Int
  , searchLocalTransportEdges :: !Int
  , searchGlobalEntryEdges :: !Int
  , searchGlobalTeleportEdges :: !Int
  , searchBankEdges :: !Int
  }
  deriving stock (Eq, Show)

data QueryTimings = QueryTimings
  { sourceAttachmentMilliseconds :: Double
  , targetAttachmentMilliseconds :: Double
  , abstractSearchMilliseconds :: Double
  , reconstructionMilliseconds :: Double
  , totalMilliseconds :: Double
  , querySearchCounters :: SearchCounters
  }
  deriving stock (Eq, Show)

instance RouteFinder Hierarchical where
  routeName _ = "hierarchical"
  findRoute hierarchical query =
    finishSearch hierarchical result
   where
    index = denseIndex hierarchical query
    result = searchHierarchy hierarchical query index (sourceAttachmentList hierarchical query) (targetDistanceMap hierarchical query)

findRouteProfiled :: Hierarchical -> Query -> IO (Route, QueryTimings)
findRouteProfiled hierarchical query = do
  started <- getMonotonicTimeNSec
  (sourceAttachments, sourceMs) <- timed
    (\attachments -> evaluate (sum [distance + unTile tile | (tile, distance) <- attachments]) >> pure attachments)
    (sourceAttachmentList hierarchical query)
  (targetDistances, targetMs) <- timed
    (\distances -> evaluate (Map.foldlWithKey' (\total tile distance -> total + unTile tile + distance) 0 distances) >> pure distances)
    (targetDistanceMap hierarchical query)
  let index = denseIndex hierarchical query
  (result, searchMs) <- timed evaluate $ searchHierarchy hierarchical query index sourceAttachments targetDistances
  (route, reconstructionMs) <- timed
    (\value -> evaluate (routeCost value + routeExpandedNodes value + length (routeSteps value)) >> pure value)
    (finishSearch hierarchical result)
  finished <- getMonotonicTimeNSec
  pure
    ( route
    , QueryTimings sourceMs targetMs searchMs reconstructionMs (milliseconds started finished) (resultCounters result)
    )

timed :: (a -> IO b) -> a -> IO (b, Double)
timed action value = do
  started <- getMonotonicTimeNSec
  result <- action value
  finished <- getMonotonicTimeNSec
  pure (result, milliseconds started finished)

milliseconds :: Word64 -> Word64 -> Double
milliseconds started finished = fromIntegral (finished - started) / 1000000

searchHierarchy
  :: Hierarchical
  -> Query
  -> DenseIndex
  -> [(Tile, Int)]
  -> Map.Map Tile Int
  -> SearchResult
searchHierarchy (Hierarchical world hierarchy) query index sourceAttachments targetDistances = runST $ do
    distances <- Mutable.replicate stateCount maxBound
    parents <- Mutable.replicate stateCount (-1)
    previous <- BoxedMutable.replicate stateCount Nothing
    Mutable.write distances start 0
    let relax kind cost state (queue, counters) (next, edgeCost, edge) = do
          let counted = edgeCounter kind counters
          case addCost cost edgeCost of
            Nothing -> pure (queue, counted)
            Just newCost -> do
              oldCost <- Mutable.read distances next
              if newCost >= oldCost
                then pure (queue, counted)
                else do
                  Mutable.write distances next newCost
                  Mutable.write parents next state
                  BoxedMutable.write previous next (Just edge)
                  pure (PQueue.insert newCost next queue, successCounter counted)
        relaxGroup cost state values (kind, edges) =
          foldM (relax kind cost state) values edges
        search queue expanded counters =
          case PQueue.minViewWithKey queue of
            Nothing -> pure (SearchFailed expanded counters)
            Just ((cost, state), rest) -> do
              known <- Mutable.read distances state
              let popped = popCounter counters
              if cost /= known
                then search rest expanded (staleCounter popped)
                else if nodeAt index (stateNode state) == QueryTarget
                  then do
                    frozenParents <- Vector.freeze parents
                    frozenPrevious <- Boxed.freeze previous
                    pure (SearchFound cost expanded frozenParents frozenPrevious state popped)
                  else do
                    (queue', counters') <- foldM (relaxGroup cost state) (rest, popped) (neighborGroups state)
                    search queue' (expanded + 1) counters'
    search (PQueue.singleton 0 start) 0 emptyCounters
   where
    stateCount = Boxed.length (denseNodes index) * 2
    start = stateId (denseQuerySource index) False
    target = queryTarget query

    neighborGroups state =
      let node = nodeAt index (stateNode state)
          banked = stateBanked state
       in
      [ (SourceEdge, sourceEdges node)
      , (TargetEdge, targetEdges node banked <> targetZero node banked)
      , (MetricEdge, metricEdges node banked)
      , (SeparatorEdge, separatorEdges node banked)
      , (LocalTransportEdge, transportEdges node banked)
      , (GlobalEntryEdge, globalEntryEdges node banked)
      , (GlobalTeleportEdge, globalEdges node banked)
      , (BankEdge, bankEdges node banked)
      ]

    sourceEdges QuerySource =
      [ (stateId node False, distance, sourceEdge tile distance)
      | (tile, distance) <- sourceAttachments
      , Just node <- [nodeForTileMaybe tile]
      ]
    sourceEdges _ = []

    sourceEdge tile distance =
      case classOf tile of
        Just (SeparatorTile _ _) -> EdgeAttach tile
        _ -> EdgeMetric distance (queryStart query) tile

    targetEdges (Terminal tile) banked =
      [ (stateId (denseQueryTarget index) banked, distance, EdgeMetric distance tile target)
      | Just distance <- [targetDistance tile]
      ]
    targetEdges _ _ = []

    metricEdges (Terminal tile) banked =
      [ (stateId otherNode banked, distance, EdgeMetric distance tile other)
      | Just leaf <- [Map.lookup tile (terminalLeaf hierarchy)]
      , Just overlay <- [Map.lookup leaf (leafOverlays hierarchy)]
      , Just adjacent <- [Map.lookup tile (leafTerminalAdjacency overlay)]
      , (other, distance) <- Map.toAscList adjacent
      , Just otherNode <- [nodeForTileMaybe other]
      ]
    metricEdges _ _ = []

    separatorEdges (Separator tile) banked =
      [ (stateId nextNode banked, 1, EdgeWalk 1 next)
      | next <- walkingNeighborsRaw world tile
      , Just nextNode <- [nodeForTileMaybe next]
      ]
    separatorEdges (Terminal tile) banked =
      [ (stateId nextNode banked, 1, EdgeWalk 1 next)
      | next <- walkingNeighborsRaw world tile
      , Just nextNode <- [nodeForTileMaybe next]
      , Separator _ <- [nodeAt index nextNode]
      ]
    separatorEdges _ _ = []

    transportEdges node banked
      | not (allowTransports query) = []
      | otherwise =
          [ (stateId destinationNode banked, transportCost transport, EdgeTransport (transportCost transport) (label transport) destination)
          | Just tile <- [tileForNode node]
          , transport <- Map.findWithDefault [] tile (worldTransports world)
          , transportType transport /= "VIRTUAL_WALL"
          , enabled transport
          , Just destination <- [destination transport]
          , Just destinationNode <- [nodeForTileMaybe destination]
          ]

    globalEntryEdges node banked
      | not (allowTransports query) || node == GlobalTeleportHub || node == QueryTarget = []
      | otherwise = [(stateId (denseGlobalHub index) banked, 0, EdgeAttach (queryStart query))]

    globalEdges GlobalTeleportHub banked =
      [ (stateId destinationNode banked, transportCost transport, EdgeTransport (transportCost transport) (label transport) destination)
      | transport <- worldGlobalTeleports world
      , enabled transport
      , Just destination <- [destination transport]
      , Just destinationNode <- [nodeForTileMaybe destination]
      ]
    globalEdges _ _ = []

    bankEdges node False
      | bankPathEnabled query
      , Just tile <- tileForNode node
      , Set.member tile (worldBanks world)
      , Just nodeId <- nodeForTileMaybe tile = [(stateId nodeId True, 0, EdgeWalk 0 tile)]
    bankEdges _ _ = []

    targetZero (Separator tile) banked
      | tile == target = [(stateId (denseQueryTarget index) banked, 0, EdgeAttach tile)]
    targetZero (Terminal tile) banked
      | tile == target = [(stateId (denseQueryTarget index) banked, 0, EdgeAttach tile)]
    targetZero _ _ = []

    targetDistance tile = Map.lookup tile targetDistances

    classOf tile = IntMap.lookup (unTile tile) (tileClasses (hierarchyPartition hierarchy))

    nodeForTileMaybe tile =
      IntMap.lookup (unTile tile) (denseTileNodes index)

    tileForNode (Terminal tile) = Just tile
    tileForNode (Separator tile) = Just tile
    tileForNode _ = Nothing

    enabled transport =
      Set.null (enabledTransportTypes query)
        || Set.member (transportType transport) (enabledTransportTypes query)
    penalty transport = Map.findWithDefault 0 (transportType transport) (transportPenalties query)
    transportCost transport = duration transport + penalty transport
    label transport = if null (displayInfo transport) then transportType transport else displayInfo transport

denseIndex :: Hierarchical -> Query -> DenseIndex
denseIndex (Hierarchical _ hierarchy) query =
  DenseIndex nodes tileNodes hubId sourceId targetId
 where
  partition = hierarchyPartition hierarchy
  classified = IntMap.unions
    [ IntMap.fromList [(unTile tile, Terminal tile) | tile <- Map.keys (terminalLeaf hierarchy)]
    , IntMap.fromList [(unTile tile, Separator tile) | tile <- Map.keys (hierarchySeparatorNodes hierarchy)]
    , IntMap.fromList
        [ (unTile tile, node)
        | tile <- [queryStart query, queryTarget query]
        , Just node <- [endpointNode partition tile]
        ]
    ]
  spatialNodes = IntMap.toAscList classified
  spatialCount = length spatialNodes
  hubId = spatialCount
  sourceId = spatialCount + 1
  targetId = spatialCount + 2
  nodes = Boxed.fromList (map snd spatialNodes <> [GlobalTeleportHub, QuerySource, QueryTarget])
  tileNodes = IntMap.fromAscList (zipWith (\(packed, _) nodeId -> (packed, nodeId)) spatialNodes [0 ..])

endpointNode :: Partition -> Tile -> Maybe Node
endpointNode partition tile =
  case IntMap.lookup (unTile tile) (tileClasses partition) of
    Just (LeafTile _) -> Just (Terminal tile)
    Just (SeparatorTile _ _) -> Just (Separator tile)
    Nothing -> Nothing

nodeAt :: DenseIndex -> Int -> Node
nodeAt index nodeId = denseNodes index Boxed.! nodeId

stateId :: Int -> Bool -> Int
stateId nodeId banked = nodeId * 2 + if banked then 1 else 0

stateNode :: Int -> Int
stateNode state = state `div` 2

stateBanked :: Int -> Bool
stateBanked state = odd state


sourceAttachmentList :: Hierarchical -> Query -> [(Tile, Int)]
sourceAttachmentList (Hierarchical world hierarchy) query =
  case classOf source of
    Just (LeafTile leaf) ->
      let terminals = maybe [] (Map.keys . leafTerminals) (Map.lookup leaf (leafOverlays hierarchy))
       in (source, 0) : leafBfs (walkingNeighborsRaw world) (leafTiles leaf) source terminals
    Just (SeparatorTile _ _) -> [(source, 0)]
    Nothing -> []
 where
  source = queryStart query
  partition = hierarchyPartition hierarchy
  classOf tile = IntMap.lookup (unTile tile) (tileClasses partition)
  leafTiles leaf = Map.findWithDefault IntSet.empty leaf (leafTileSets partition)

targetDistanceMap :: Hierarchical -> Query -> Map.Map Tile Int
targetDistanceMap (Hierarchical world hierarchy) query =
  case classOf target of
    Just (LeafTile leaf) ->
      let terminals = maybe [] (Map.keys . leafTerminals) (Map.lookup leaf (leafOverlays hierarchy))
          source = queryStart query
          candidates = if classOf source == Just (LeafTile leaf) then source : terminals else terminals
       in Map.fromList ((target, 0) : leafBfs (walkingNeighborsRaw world) (leafTiles leaf) target candidates)
    _ -> Map.empty
 where
  target = queryTarget query
  partition = hierarchyPartition hierarchy
  classOf tile = IntMap.lookup (unTile tile) (tileClasses partition)
  leafTiles leaf = Map.findWithDefault IntSet.empty leaf (leafTileSets partition)

finishSearch :: Hierarchical -> SearchResult -> Route
finishSearch _ (SearchFailed expanded _) = Route maxBound expanded []
finishSearch (Hierarchical world hierarchy) (SearchFound cost expanded parents previous state _) =
  let edges = collect state []
      concrete = map expand edges
      concreteCost = sum (map fst concrete)
      steps = concatMap snd concrete
   in assert (concreteCost == cost) (Route cost expanded steps)
 where
  partition = hierarchyPartition hierarchy
  classOf tile = IntMap.lookup (unTile tile) (tileClasses partition)
  leafTiles leaf = Map.findWithDefault IntSet.empty leaf (leafTileSets partition)

  collect current edges =
    let parent = parents Vector.! current
     in if parent < 0
          then edges
          else case previous Boxed.! current of
            Nothing -> error "hierarchical predecessor has no edge"
            Just edge -> collect parent (edge : edges)

  expand (EdgeWalk edgeCost tile) = (edgeCost, [Walk tile])
  expand (EdgeTransport edgeCost name tile) = (edgeCost, [UseTransport name tile])
  expand (EdgeAttach _) = (0, [])
  expand (EdgeMetric edgeCost from to) =
    case (classOf from, classOf to) of
      (Just (LeafTile leaf), Just (LeafTile same))
        | leaf == same ->
            case reconstructLeafPath (walkingNeighborsRaw world) (leafTiles leaf) from to of
              Just tiles -> assert (length tiles - 1 == edgeCost) (edgeCost, map Walk (drop 1 tiles))
              Nothing -> error "hierarchical metric edge cannot be reconstructed"
      _ -> error "hierarchical metric edge crosses leaf boundary"

resultCounters :: SearchResult -> SearchCounters
resultCounters (SearchFailed _ counters) = counters
resultCounters (SearchFound _ _ _ _ _ counters) = counters

emptyCounters :: SearchCounters
emptyCounters = SearchCounters 0 0 0 0 0 0 0 0 0 0 0 0

popCounter, staleCounter, successCounter :: SearchCounters -> SearchCounters
popCounter counters = counters {searchQueuePops = searchQueuePops counters + 1}
staleCounter counters = counters {searchStalePops = searchStalePops counters + 1}
successCounter counters = counters {searchSuccessfulRelaxations = searchSuccessfulRelaxations counters + 1}

edgeCounter :: EdgeKind -> SearchCounters -> SearchCounters
edgeCounter kind counters =
  let counted = counters {searchEdgesConsidered = searchEdgesConsidered counters + 1}
   in case kind of
        SourceEdge -> counted {searchSourceEdges = searchSourceEdges counted + 1}
        TargetEdge -> counted {searchTargetEdges = searchTargetEdges counted + 1}
        MetricEdge -> counted {searchMetricEdges = searchMetricEdges counted + 1}
        SeparatorEdge -> counted {searchSeparatorEdges = searchSeparatorEdges counted + 1}
        LocalTransportEdge -> counted {searchLocalTransportEdges = searchLocalTransportEdges counted + 1}
        GlobalEntryEdge -> counted {searchGlobalEntryEdges = searchGlobalEntryEdges counted + 1}
        GlobalTeleportEdge -> counted {searchGlobalTeleportEdges = searchGlobalTeleportEdges counted + 1}
        BankEdge -> counted {searchBankEdges = searchBankEdges counted + 1}

addCost :: Int -> Int -> Maybe Int
addCost a b
  | b < 0 || a > maxBound - b = Nothing
  | otherwise = Just (a + b)

leafBfs :: (Tile -> [Tile]) -> IntSet.IntSet -> Tile -> [Tile] -> [(Tile, Int)]
leafBfs neighbours tiles source targets = runST $ do
  let indexed = IntMap.fromList (zip (IntSet.toList tiles) [0 ..])
      tileByIndex = Vector.fromList (IntSet.toList tiles)
      capacity = max 1 (IntSet.size tiles)
  queue <- Mutable.new capacity
  distances <- Mutable.replicate capacity (-1 :: Int)
  let visit currentDistance writeIx next =
        case IntMap.lookup (unTile next) indexed of
          Nothing -> pure writeIx
          Just index -> do
            existing <- Mutable.read distances index
            if existing >= 0
              then pure writeIx
              else do
                Mutable.write distances index (currentDistance + 1)
                Mutable.write queue writeIx index
                pure (writeIx + 1)
      flood writeIx readIx
        | writeIx == readIx = pure ()
        | otherwise = do
            index <- Mutable.read queue readIx
            currentDistance <- Mutable.read distances index
            nextWrite <- foldM (visit currentDistance) writeIx (neighbours (Tile (tileByIndex Vector.! index)))
            flood nextWrite (readIx + 1)
      targetDistance tile =
        case IntMap.lookup (unTile tile) indexed of
          Nothing -> pure []
          Just index -> do
            value <- Mutable.read distances index
            pure [(tile, value) | value >= 0]
  case IntMap.lookup (unTile source) indexed of
    Nothing -> pure []
    Just start -> do
      Mutable.write queue 0 start
      Mutable.write distances start 0
      flood 1 0
      fmap concat (forM targets targetDistance)
