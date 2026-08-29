module ShortestPath.Exact.Hierarchical
  ( Hierarchical(..)
  , QueryTimings(..)
  , findRouteProfiled
  ) where

import Control.Exception (assert, evaluate)
import Control.Monad (foldM, forM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO.Unsafe (unsafePerformIO)

import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Preprocess
import ShortestPath.Hierarchy.Types
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

data Hierarchical = Hierarchical World Hierarchy

data State = State Node Bool
  deriving stock (Eq, Ord, Show)

data Edge
  = EdgeWalk Int Tile
  | EdgeTransport Int String Tile
  | EdgeMetric Int Tile Tile
  | EdgeAttach Tile
  deriving stock (Eq, Show)

data Prev = Prev State Edge
  deriving stock (Eq, Show)

data SearchResult
  = SearchFailed Int
  | SearchFound Int Int (Map.Map State Prev) State

data QueryTimings = QueryTimings
  { sourceAttachmentMilliseconds :: Double
  , targetAttachmentMilliseconds :: Double
  , abstractSearchMilliseconds :: Double
  , reconstructionMilliseconds :: Double
  , totalMilliseconds :: Double
  }
  deriving stock (Eq, Show)

instance RouteFinder Hierarchical where
  routeName _ = "hierarchical"
  findRoute hierarchical query =
    finishSearch hierarchical query result
   where
    sourceAttachments = sourceAttachmentList hierarchical query
    targetDistances = targetDistanceMap hierarchical query
    result = searchHierarchy hierarchical query sourceAttachments targetDistances

findRouteProfiled :: Hierarchical -> Query -> IO (Route, QueryTimings)
findRouteProfiled hierarchical query = do
  started <- getMonotonicTimeNSec
  (sourceAttachments, sourceMs) <- timed
    (\attachments -> evaluate (sum [distance + unTile tile | (tile, distance) <- attachments]) >> pure attachments)
    (sourceAttachmentList hierarchical query)
  (targetDistances, targetMs) <- timed
    (\distances -> evaluate (Map.foldlWithKey' (\total tile distance -> total + unTile tile + distance) 0 distances) >> pure distances)
    (targetDistanceMap hierarchical query)
  (result, searchMs) <- timed evaluate $ searchHierarchy hierarchical query sourceAttachments targetDistances
  (route, reconstructionMs) <- timed
    (\value -> evaluate (routeCost value + routeExpandedNodes value + length (routeSteps value)) >> pure value)
    (finishSearch hierarchical query result)
  finished <- getMonotonicTimeNSec
  pure
    ( route
    , QueryTimings sourceMs targetMs searchMs reconstructionMs (milliseconds started finished)
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
  -> [(Tile, Int)]
  -> Map.Map Tile Int
  -> SearchResult
searchHierarchy (Hierarchical world hierarchy) query sourceAttachments targetDistances =
    search (Set.singleton (0, start)) (Map.singleton start 0) Map.empty Set.empty 0
   where
    start = State QuerySource False
    target = queryTarget query

    search queue best previous settled expanded =
      case Set.minView queue of
        Nothing -> SearchFailed expanded
        Just ((cost, state), rest)
          | Set.member state settled -> search rest best previous settled expanded
          | State node _ <- state, node == QueryTarget ->
              SearchFound cost expanded previous state
          | otherwise ->
              let settled' = Set.insert state settled
                  (queue', best', previous') =
                    foldl (relax cost state) (rest, best, previous) (neighbors state)
               in search queue' best' previous' settled' (expanded + 1)

    relax cost state (queue, best, previous) (next, edgeCost, edge) =
      case addCost cost edgeCost of
        Nothing -> (queue, best, previous)
        Just newCost
          | newCost < Map.findWithDefault maxBound next best ->
              ( Set.insert (newCost, next) queue
              , Map.insert next newCost best
              , Map.insert next (Prev state edge) previous
              )
          | otherwise -> (queue, best, previous)

    neighbors (State node banked) =
      sourceEdges node
        <> targetEdges node banked
        <> metricEdges node banked
        <> separatorEdges node banked
        <> transportEdges node banked
        <> globalEntryEdges node banked
        <> globalEdges node banked
        <> bankEdges node banked
        <> targetZero node banked

    sourceEdges QuerySource =
      [ (State (nodeForTile tile) False, distance, sourceEdge tile distance)
      | (tile, distance) <- sourceAttachments
      , Just _ <- [nodeForTileMaybe tile]
      ]
    sourceEdges _ = []

    sourceEdge tile distance =
      case classOf tile of
        Just (SeparatorTile _ _) -> EdgeAttach tile
        _ -> EdgeMetric distance (queryStart query) tile

    targetEdges (Terminal tile) banked =
      [ (State QueryTarget banked, distance, EdgeMetric distance tile target)
      | Just distance <- [targetDistance tile]
      ]
    targetEdges _ _ = []

    metricEdges (Terminal tile) banked =
      [ (State (Terminal other) banked, distance, EdgeMetric distance tile other)
      | Just leaf <- [Map.lookup tile (terminalLeaf hierarchy)]
      , Just overlay <- [Map.lookup leaf (leafOverlays hierarchy)]
      , other <- Map.keys (leafTerminals overlay)
      , other /= tile
      , Just distance <- [leafDistance overlay tile other]
      ]
    metricEdges _ _ = []

    separatorEdges (Separator tile) banked =
      [ (State (nodeForTile next) banked, 1, EdgeWalk 1 next)
      | next <- walkingNeighborsRaw world tile
      , Just _ <- [nodeForTileMaybe next]
      ]
    separatorEdges (Terminal tile) banked =
      [ (State (Separator next) banked, 1, EdgeWalk 1 next)
      | next <- walkingNeighborsRaw world tile
      , Just (Separator _) <- [nodeForTileMaybe next]
      ]
    separatorEdges _ _ = []

    transportEdges node banked
      | not (allowTransports query) = []
      | otherwise =
          [ (State (nodeForTile destination) banked, transportCost transport, EdgeTransport (transportCost transport) (label transport) destination)
          | Just tile <- [tileForNode node]
          , transport <- Map.findWithDefault [] tile (worldTransports world)
          , transportType transport /= "VIRTUAL_WALL"
          , enabled transport
          , Just destination <- [destination transport]
          , Just _ <- [nodeForTileMaybe destination]
          ]

    globalEntryEdges node banked
      | not (allowTransports query) || node == GlobalTeleportHub || node == QueryTarget = []
      | otherwise = [(State GlobalTeleportHub banked, 0, EdgeAttach (queryStart query))]

    globalEdges GlobalTeleportHub banked =
      [ (State (nodeForTile destination) banked, transportCost transport, EdgeTransport (transportCost transport) (label transport) destination)
      | transport <- worldGlobalTeleports world
      , enabled transport
      , Just destination <- [destination transport]
      , Just _ <- [nodeForTileMaybe destination]
      ]
    globalEdges _ _ = []

    bankEdges node False
      | bankPathEnabled query
      , Just tile <- tileForNode node
      , Set.member tile (worldBanks world) = [(State node True, 0, EdgeWalk 0 tile)]
    bankEdges _ _ = []

    targetZero (Separator tile) banked
      | tile == target = [(State QueryTarget banked, 0, EdgeAttach tile)]
    targetZero (Terminal tile) banked
      | tile == target = [(State QueryTarget banked, 0, EdgeAttach tile)]
    targetZero _ _ = []

    targetDistance tile = Map.lookup tile targetDistances

    classOf tile = IntMap.lookup (unTile tile) (tileClasses (hierarchyPartition hierarchy))

    nodeForTileMaybe tile =
      case classOf tile of
        Just (LeafTile _) -> Just (Terminal tile)
        Just (SeparatorTile _ _) -> Just (Separator tile)
        Nothing -> Nothing
    nodeForTile tile = maybe (Terminal tile) id (nodeForTileMaybe tile)

    tileForNode (Terminal tile) = Just tile
    tileForNode (Separator tile) = Just tile
    tileForNode _ = Nothing

    enabled transport =
      Set.null (enabledTransportTypes query)
        || Set.member (transportType transport) (enabledTransportTypes query)
    penalty transport = Map.findWithDefault 0 (transportType transport) (transportPenalties query)
    transportCost transport = duration transport + penalty transport
    label transport = if null (displayInfo transport) then transportType transport else displayInfo transport


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

finishSearch :: Hierarchical -> Query -> SearchResult -> Route
finishSearch _ _ (SearchFailed expanded) = Route maxBound expanded []
finishSearch (Hierarchical world hierarchy) _ (SearchFound cost expanded previous state) =
  let (concreteCost, steps) = reconstruct state
   in assert (concreteCost == cost) (Route cost expanded steps)
 where
  partition = hierarchyPartition hierarchy
  classOf tile = IntMap.lookup (unTile tile) (tileClasses partition)
  leafTiles leaf = Map.findWithDefault IntSet.empty leaf (leafTileSets partition)

  reconstruct current =
    case Map.lookup current previous of
      Nothing -> (0, [])
      Just (Prev parent edge) ->
        let (parentCost, parentSteps) = reconstruct parent
            (edgeCost, edgeSteps) = expand edge
         in (parentCost + edgeCost, parentSteps <> edgeSteps)

  expand (EdgeWalk edgeCost tile) = (edgeCost, [Walk tile])
  expand (EdgeTransport edgeCost name tile) = (edgeCost, [UseTransport name tile])
  expand (EdgeAttach _) = (0, [])
  expand (EdgeMetric edgeCost from to) =
    case (classOf from, classOf to) of
      (Just (LeafTile leaf), Just (LeafTile same))
        | leaf == same ->
            case reconstructLeafPathPure (walkingNeighborsRaw world) (leafTiles leaf) from to of
              Just path -> assert (length path - 1 == edgeCost) (edgeCost, map Walk (drop 1 path))
              Nothing -> assert False (0, [])
      _ -> assert False (0, [])

addCost :: Int -> Int -> Maybe Int
addCost a b
  | b < 0 || a > maxBound - b = Nothing
  | otherwise = Just (a + b)

leafBfs :: (Tile -> [Tile]) -> IntSet.IntSet -> Tile -> [Tile] -> [(Tile, Int)]
leafBfs neighbours tiles source targets = unsafePerformIO $ do
  let indexed = IntMap.fromList (zip (IntSet.toList tiles) [0 ..])
      tileByIndex = Vector.fromList (IntSet.toList tiles)
      capacity = max 1 (IntSet.size tiles)
  queue <- Mutable.new capacity
  distances <- Mutable.replicate capacity (-1 :: Int)
  case IntMap.lookup (unTile source) indexed of
    Nothing -> pure []
    Just start -> do
      Mutable.write queue 0 start
      Mutable.write distances start 0
      flood indexed tileByIndex queue distances 1 0
      fmap concat (forM targets (distance indexed distances))
 where
  flood indexed tileByIndex queue distances writeIx readIx
    | writeIx == readIx = pure ()
    | otherwise = do
        index <- Mutable.read queue readIx
        currentDistance <- Mutable.read distances index
        nextWrite <- foldM (visit indexed queue distances currentDistance) writeIx (neighbours (Tile (tileByIndex Vector.! index)))
        flood indexed tileByIndex queue distances nextWrite (readIx + 1)
  visit indexed queue distances currentDistance writeIx next =
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
  distance indexed distances tile =
    case IntMap.lookup (unTile tile) indexed of
      Nothing -> pure []
      Just index -> do
        value <- Mutable.read distances index
        pure [(tile, value) | value >= 0]

reconstructLeafPathPure :: (Tile -> [Tile]) -> IntSet.IntSet -> Tile -> Tile -> Maybe [Tile]
reconstructLeafPathPure neighbours tiles source target =
  unsafePerformIO (reconstructLeafPath neighbours tiles source target)
