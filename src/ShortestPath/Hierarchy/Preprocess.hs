{-# LANGUAGE MonoLocalBinds #-}

module ShortestPath.Hierarchy.Preprocess
  ( preprocessHierarchy
  , preprocessHierarchyWith
  , discoverLeafTerminals
  , leafDistance
  , canonicalPair
  , reconstructLeafPath
  ) where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.ST (runST)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO (hFlush, stdout)

import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Types
import ShortestPath.Tile
import ShortestPath.Transport (Transport(..))
import ShortestPath.World

preprocessHierarchy :: Partition -> World -> IO Hierarchy
preprocessHierarchy partition world =
  preprocessHierarchyWith partition (walkingNeighborsRaw world) roles
 where
  locals =
    [ transport
    | transports <- Map.elems (worldTransports world)
    , transport <- transports
    , transportType transport /= "VIRTUAL_WALL"
    ]
  roles =
    TerminalRoles
      { roleBanks = worldBanks world
      , roleLocalOrigins = Set.fromList [tile | transport <- locals, Just tile <- [origin transport]]
      , roleLocalDestinations = Set.fromList [tile | transport <- locals, Just tile <- [destination transport]]
      , roleGlobalDestinations = Set.fromList [tile | transport <- worldGlobalTeleports world, Just tile <- [destination transport]]
      }

preprocessHierarchyWith
  :: Partition
  -> (Tile -> [Tile])
  -> TerminalRoles
  -> IO Hierarchy
preprocessHierarchyWith partition neighbours roles = do
  let leaves = Map.toList (leafTileSets partition)
      leafCount = length leaves
      separators = Map.fromList [(tile, Separator tile) | n <- IntSet.toList (separatorTileSet partition), let tile = Tile n]
  overlays <- forM (zip [1 :: Int ..] leaves) $ \(position, (leaf, tiles)) -> do
    started <- getCurrentTime
    let terminals = discoverLeafTerminals partition neighbours roles leaf tiles
    (distances, expanded) <- metricClosure neighbours tiles terminals
    finished <- getCurrentTime
    let stats = LeafStats
          { leafTileCount = IntSet.size tiles
          , leafTerminalCount = Map.size terminals
          , leafGatewayCount = length [() | kinds <- Map.elems terminals, RegionGateway `Set.member` kinds]
          , leafBfsCount = max 0 (Map.size terminals - 1)
          , leafExpandedTiles = expanded
          , leafDistanceEntries = Map.size distances
          , leafElapsedMilliseconds = round (realToFrac (diffUTCTime finished started) * (1000 :: Double))
          }
        overlay = LeafOverlay terminals distances (terminalAdjacency distances) stats
    when (leafTileCount stats >= 10000 || leafTerminalCount stats >= 50 || position `mod` 100 == 0 || position == leafCount) $ do
      putStrLn ("preprocessed leaf " <> show position <> "/" <> show leafCount <> " " <> show leaf <> ": " <> show stats)
      hFlush stdout
    pure (leaf, overlay)
  let overlayMap = Map.fromList overlays
      terminals = Map.fromList
        [ (tile, leaf)
        | (leaf, overlay) <- overlays
        , tile <- Map.keys (leafTerminals overlay)
        ]
  validateEndpoints partition roles (Map.keysSet terminals)
  pure Hierarchy
    { hierarchyPartition = partition
    , leafOverlays = overlayMap
    , terminalLeaf = terminals
    , hierarchySeparatorNodes = separators
    , hierarchyStats = Map.map leafStats overlayMap
    }

discoverLeafTerminals
  :: Partition
  -> (Tile -> [Tile])
  -> TerminalRoles
  -> LeafId
  -> IntSet.IntSet
  -> Map.Map Tile (Set.Set TerminalKind)
discoverLeafTerminals partition neighbours roles leaf tiles =
  Map.fromListWith Set.union
    [ (tile, kinds)
    | n <- IntSet.toList tiles
    , let tile = Tile n
    , let kinds = explicitKinds tile `Set.union` gatewayKind tile
    , not (Set.null kinds)
    ]
 where
  explicitKinds tile = Set.fromList
    [ kind
    | (kind, locations) <-
        [ (BankTerminal, roleBanks roles)
        , (LocalTransportOrigin, roleLocalOrigins roles)
        , (LocalTransportDestination, roleLocalDestinations roles)
        , (GlobalTeleportDestination, roleGlobalDestinations roles)
        ]
    , tile `Set.member` locations
    , tileClass tile == Just (LeafTile leaf)
    ]
  gatewayKind tile
    | any isSeparator (neighbours tile) = Set.singleton RegionGateway
    | otherwise = Set.empty
  isSeparator next = IntSet.member (unTile next) (separatorTileSet partition)
  tileClass tile = IntMap.lookup (unTile tile) (tileClasses partition)

validateEndpoints
  :: Partition
  -> TerminalRoles
  -> Set.Set Tile
  -> IO ()
validateEndpoints partition roles terminals =
  forM_ (Set.toList (roleLocalOrigins roles `Set.union` roleLocalDestinations roles)) $ \tile ->
    case IntMap.lookup (unTile tile) (tileClasses partition) of
      Nothing -> pure ()
      Just (SeparatorTile _ _) -> pure ()
      Just (LeafTile _) ->
        when (tile `Set.notMember` terminals) $
          fail ("local transport endpoint is not indexed as a terminal: " <> coordinateText tile)

metricClosure
  :: (Tile -> [Tile])
  -> IntSet.IntSet
  -> Map.Map Tile (Set.Set TerminalKind)
  -> IO (Map.Map (Tile, Tile) Int, Int)
metricClosure neighbours tiles terminals = do
  let packedTiles = IntSet.toAscList tiles
      indexed = IntMap.fromList (zip packedTiles [0 ..])
      terminalList = Map.keys terminals
      terminalPacked = Vector.fromList (map unTile terminalList)
      terminalIndices = Vector.fromList [indexed IntMap.! unTile tile | tile <- terminalList]
      terminalCount = Vector.length terminalIndices
      adjacencyLists =
        [ [index | next <- neighbours (Tile packed), Just index <- [IntMap.lookup (unTile next) indexed]]
        | packed <- packedTiles
        ]
      offsets = Vector.fromList (scanl (+) 0 (map length adjacencyLists))
      edges = Vector.fromList (concat adjacencyLists)
      terminalOrdinal = Vector.create $ do
        ordinals <- Mutable.replicate (max 1 (length packedTiles)) (-1 :: Int)
        forM_ (zip [0 ..] (Vector.toList terminalIndices)) $ \(ordinal, index) ->
          Mutable.write ordinals index ordinal
        pure ordinals
  queue <- Mutable.new (max 1 (length packedTiles))
  distances <- Mutable.new (max 1 (length packedTiles))
  generations <- Mutable.replicate (max 1 (length packedTiles)) (0 :: Int)
  results <- forM [0 .. terminalCount - 1] $ \sourceOrdinal ->
    bfs offsets edges terminalOrdinal queue distances generations terminalIndices terminalPacked sourceOrdinal
  pure (Map.fromDistinctAscList (concatMap fst results), sum (map snd results))
 where
  bfs offsets edges terminalOrdinal queue distances generations terminalIndices terminalPacked sourceOrdinal = do
    let generation = sourceOrdinal + 1
        start = terminalIndices Vector.! sourceOrdinal
        remaining = Vector.length terminalIndices - sourceOrdinal - 1
    Mutable.write queue 0 start
    Mutable.write distances start 0
    Mutable.write generations start generation
    (expanded, _) <- flood offsets edges terminalOrdinal queue distances generations generation sourceOrdinal 1 0 remaining 0
    entries <- fmap concat $ forM [sourceOrdinal + 1 .. Vector.length terminalIndices - 1] $ \targetOrdinal -> do
      let targetIndex = terminalIndices Vector.! targetOrdinal
      seen <- Mutable.read generations targetIndex
      if seen /= generation
        then pure []
        else do
          distance <- Mutable.read distances targetIndex
          pure [((Tile (terminalPacked Vector.! sourceOrdinal), Tile (terminalPacked Vector.! targetOrdinal)), distance)]
    pure (entries, expanded)

  flood offsets edges ordinals queue distances generations generation sourceOrdinal writeIx readIx remaining expanded
    | remaining == 0 || readIx == writeIx = pure (expanded, writeIx)
    | otherwise = do
        current <- Mutable.read queue readIx
        distance <- Mutable.read distances current
        (writeIx', remaining') <- visitEdges
          edges ordinals queue distances generations generation sourceOrdinal distance
          (offsets Vector.! current) (offsets Vector.! (current + 1)) writeIx remaining
        flood offsets edges ordinals queue distances generations generation sourceOrdinal writeIx' (readIx + 1) remaining' (expanded + 1)

  visitEdges edges ordinals queue distances generations generation sourceOrdinal distance edgeIx edgeEnd writeIx remaining
    | edgeIx == edgeEnd = pure (writeIx, remaining)
    | otherwise = do
        let next = edges Vector.! edgeIx
        seen <- Mutable.read generations next
        if seen == generation
          then visitEdges edges ordinals queue distances generations generation sourceOrdinal distance (edgeIx + 1) edgeEnd writeIx remaining
          else do
            Mutable.write generations next generation
            Mutable.write distances next (distance + 1)
            Mutable.write queue writeIx next
            let ordinal = ordinals Vector.! next
                remaining' = if ordinal > sourceOrdinal then remaining - 1 else remaining
            visitEdges edges ordinals queue distances generations generation sourceOrdinal distance (edgeIx + 1) edgeEnd (writeIx + 1) remaining'

leafDistance :: LeafOverlay -> Tile -> Tile -> Maybe Int
leafDistance _ a b | a == b = Just 0
leafDistance overlay a b = Map.lookup (canonicalPair a b) (leafDistances overlay)

terminalAdjacency :: Map.Map (Tile, Tile) Int -> Map.Map Tile (Map.Map Tile Int)
terminalAdjacency distances =
  Map.map (Map.fromAscList . sortEntries) (Map.fromListWith (<>) entries)
 where
  entries =
    [ (source, [(target, distance)])
    | ((left, right), distance) <- Map.toAscList distances
    , (source, target) <- [(left, right), (right, left)]
    ]
  sortEntries = sortOn fst

canonicalPair :: Ord a => a -> a -> (a, a)
canonicalPair a b = if a <= b then (a, b) else (b, a)

reconstructLeafPath
  :: (Tile -> [Tile])
  -> IntSet.IntSet
  -> Tile
  -> Tile
  -> Maybe [Tile]
reconstructLeafPath neighbours tiles source target = runST $ do
  let indexed = IntMap.fromList (zip (IntSet.toList tiles) [0 ..])
      tileByIndex = Vector.fromList (IntSet.toList tiles)
  queue <- Mutable.new (max 1 (IntSet.size tiles))
  parents <- Mutable.replicate (max 1 (IntSet.size tiles)) (-1 :: Int)
  seen <- Mutable.replicate (max 1 (IntSet.size tiles)) False
  let visit parent writeIx next = do
        let index = indexed IntMap.! unTile next
        already <- Mutable.read seen index
        if already then pure writeIx else do
          Mutable.write seen index True
          Mutable.write parents index parent
          Mutable.write queue writeIx index
          pure (writeIx + 1)
      search writeIx readIx goal
        | readIx == writeIx = pure False
        | otherwise = do
            index <- Mutable.read queue readIx
            if index == goal
              then pure True
              else do
                let tile = Tile (tileByIndex Vector.! index)
                    next = [n | n <- neighbours tile, Just _ <- [IntMap.lookup (unTile n) indexed]]
                writeIx' <- foldM (visit index) writeIx next
                search writeIx' (readIx + 1) goal
      unwind goal = go goal []
       where
        go i acc
          | i < 0 = pure acc
          | otherwise = do
              parent <- Mutable.read parents i
              go parent (Tile (tileByIndex Vector.! i) : acc)
  case (IntMap.lookup (unTile source) indexed, IntMap.lookup (unTile target) indexed) of
    (Just start, Just goal) -> do
      Mutable.write queue 0 start
      Mutable.write seen start True
      found <- search 1 0 goal
      if not found then pure Nothing else Just <$> unwind goal
    _ -> pure Nothing
