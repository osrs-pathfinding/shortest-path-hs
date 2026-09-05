module Main (main) where

import Data.Bits (setBit)
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Exact.Hierarchical
import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.RawDijkstra
import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Preprocess
import ShortestPath.Hierarchy.Types
import ShortestPath.Heuristic.Region
import ShortestPath.Pathfinder
import ShortestPath.Requirements
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  let (world, partition, roles, tiles) = synthetic
      defaults = defaultQuery (tA0 tiles) (tA1 tiles)
  assert (not (Set.member "SEASONAL_TRANSPORTS" (enabledTransportTypes defaults)))
  assert (Set.member "TELEPORTATION_ITEM" (enabledTransportTypes defaults))
  hierarchy <- preprocessHierarchyWith partition (walkingNeighborsRaw world) roles
  assert (decode (encode hierarchy) == hierarchy)
  checkPreprocess hierarchy world partition tiles
  let hierarchical = buildHierarchical world hierarchy
      raw = RawDijkstra world
      exactValues = regionValues
        (buildRegionGraph world hierarchy)
        False
        Set.empty
        (tA25 tiles)
        (Map.singleton (tA25 tiles) 0)
  assert (tileLowerBound exactValues False (tA0 tiles) == 25)
  tileAStar <- buildTileAStar world
  mapM_ (checkRoute raw tileAStar hierarchical world) (cases tiles)
  regionTable <- buildRegionTable (buildRegionGraph world hierarchy)
  assert (tableLowerBound regionTable False [LeafId 1 "a"] [LeafId 1 "d"] < tableLowerBound regionTable True [LeafId 1 "a"] [LeafId 1 "d"])
  let precomputed = buildHierarchicalWithRegionTable world hierarchy regionTable
  mapM_ (checkRoute raw tileAStar precomputed world) (cases tiles)
  let profiledQuery = walkingQuery (tA3 tiles) (tA8 tiles)
  (tracedRoute, _, expandedTiles, heuristicRegions, heuristicTiles) <- findRouteProfiledWithOptions True True hierarchical profiledQuery
  (dijkstraRoute, _, _, noHeuristicRegions, noHeuristicTiles) <- findRouteProfiledWithOptions False False hierarchical profiledQuery
  assert (routeCost tracedRoute == routeCost dijkstraRoute)
  assert (routeCost tracedRoute == 5)
  assert (not (null expandedTiles))
  assert (not (null heuristicRegions))
  assert (not (null heuristicTiles))
  assert (lookup (tA8 tiles) heuristicTiles == Just 0)
  assert (null noHeuristicRegions)
  assert (null noHeuristicTiles)
  let globalQuery = query (tA3 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_GLOBAL") False
      rawGlobalRoute = findRoute raw globalQuery
      tileGlobalRoute = findRoute tileAStar globalQuery
  (globalRoute, globalTimings) <- findRouteProfiled hierarchical globalQuery
  assert (routeSteps rawGlobalRoute == [UseTransport "SYNTHETIC_GLOBAL" (tD1 tiles)])
  assert (routeSteps tileGlobalRoute == [UseTransport "SYNTHETIC_GLOBAL" (tD1 tiles)])
  assert (routeCost globalRoute == 4)
  assert (routeSteps globalRoute == [UseTransport "SYNTHETIC_GLOBAL" (tD1 tiles)])
  assert (searchGlobalEntryEdges (querySearchCounters globalTimings) == 1)
  checkReversePathDebug tileAStar tiles

synthetic :: (World, Partition, TerminalRoles, Tiles)
synthetic =
  ( World (CollisionMap (Map.singleton (1, 1) collisionBytes)) transports globals banks
  , partition
  , roles
  , Tiles a0 a1 a3 a8 a25 b1 c0 d0 d1 e0 s0 s1 s2
  )
 where
  a0 = packTile 100 100 0
  a1 = packTile 101 100 0
  a3 = packTile 103 100 0
  a8 = packTile 108 100 0
  a25 = packTile 125 100 0
  a = [packTile x 100 0 | x <- [100 .. 125]]
  s0 = packTile 100 99 0
  s1 = packTile 101 99 0
  s2 = packTile 102 99 0
  b0 = packTile 103 99 0
  b1 = packTile 104 99 0
  c0 = packTile 110 110 0
  c1 = packTile 111 110 0
  d0 = packTile 110 111 0
  d1 = packTile 111 111 0
  d2 = packTile 110 112 0
  d3 = packTile 111 112 0
  e0 = packTile 109 110 0
  allEdges =
    [(packTile x 100 0, 1) | x <- [100 .. 124]]
      <> [(s0, 0), (s0, 1), (s1, 1), (s2, 1), (b0, 1)]
      <> [(c0, 1), (c0, 0), (d0, 0), (d1, 0)]
  collisionBytes = BL.pack [byteAt i | i <- [0 .. 8191]]
  byteAt i = foldr setBitIf 0 [bitIndex | (bit, bitIndex) <- flags, bit `div` 8 == i]
  setBitIf bitIndex byte = setBit byte (bitIndex `mod` 8)
  flags = [(flagOffset tile flag, flagOffset tile flag `mod` 8) | (tile, flag) <- allEdges]
  flagOffset tile flag =
    let (x, y, p) = unpackTile tile
     in ((p * 4096 + (y - 64) * 64 + (x - 64)) * 2 + flag)
  assignments =
    [PartitionAssignment 1 tile "a" "leaf" 1 | tile <- a]
      <> [PartitionAssignment 1 s "separator" "separator" 0 | s <- [s0, s1, s2]]
      <> [PartitionAssignment 1 tile "b" "leaf" 1 | tile <- [b0, b1]]
      <> [PartitionAssignment 1 tile "c" "leaf" 1 | tile <- [c0, c1]]
      <> [PartitionAssignment 1 tile "d" "leaf" 1 | tile <- [d0, d1, d2, d3]]
  ownerTiles = a <> [s0, s1, s2, b0, b1, c0, c1, d0, d1, d2, d3]
  owner = IntMap.fromList [(unTile tile, 1) | tile <- ownerTiles]
  partition = either (error . ("synthetic partition: " <>)) id
    (partitionFromAssignments owner (IntSet.singleton 1) assignments)
  roles = TerminalRoles
    { roleBanks = Set.singleton a0
    , roleLocalOrigins = Set.fromList [a0, b1, c1, e0]
    , roleLocalDestinations = Set.fromList [a1, c0, a25]
    , roleGlobalDestinations = Set.singleton d1
    }
  transports = Map.fromListWith (<>)
    [ (a0, [ local "SYNTHETIC_DIRECT" a0 a1 10
           , local "SYNTHETIC_LONG" a0 d1 20
           , localReq "SYNTHETIC_BANK_LOCAL_AT_BANK" a0 d1 5 (ItemOne (ItemTerm "999" 1))
           ])
    , (b1, [local "SYNTHETIC_BOAT" b1 c0 2])
    , (c1, [local "SYNTHETIC_RETURN" c1 a25 1])
    , (e0, [local "SYNTHETIC_RING" e0 c0 2, localReq "SYNTHETIC_BANK_LOCAL" e0 d1 5 (ItemOne (ItemTerm "999" 1))])
    ]
  globals =
    [ global "SYNTHETIC_GLOBAL" d1 4 (Just (ItemOne (ItemTerm "13393" 1)))
    , global "SYNTHETIC_BANK_GLOBAL" d1 3 (Just (ItemOne (ItemTerm "999" 1)))
    ]
  banks = Set.singleton a0

data Tiles = Tiles
  { tA0 :: Tile, tA1 :: Tile, tA3 :: Tile, tA8 :: Tile, tA25 :: Tile
  , tB1 :: Tile, tC0 :: Tile, tD0 :: Tile, tD1 :: Tile, tE0 :: Tile
  , tS0 :: Tile, tS1 :: Tile, tS2 :: Tile
  }

local :: String -> Tile -> Tile -> Int -> Transport
local kind from to cost = Transport kind (Just from) (Just to) cost kind "" False Nothing [] Nothing [] [] [] "synthetic"

localReq :: String -> Tile -> Tile -> Int -> ItemExpr -> Transport
localReq kind from to cost itemReq = Transport kind (Just from) (Just to) cost kind "" False Nothing [] (Just itemReq) [] [] [] "synthetic"

global :: String -> Tile -> Int -> Maybe ItemExpr -> Transport
global kind to cost itemReq = Transport kind Nothing (Just to) cost kind "" False Nothing [] itemReq [] [] [] "synthetic"

checkPreprocess :: Hierarchy -> World -> Partition -> Tiles -> IO ()
checkPreprocess hierarchy world partition tiles = do
  let leafA = LeafId 1 "a"
      overlay = must "leaf a" (Map.lookup leafA (leafOverlays hierarchy))
      kinds = must "duplicate terminal roles" (Map.lookup (tA0 tiles) (leafTerminals overlay))
      expected = Set.fromList [BankTerminal, LocalTransportOrigin, RegionGateway]
  assert (Map.size (leafTileSets partition) == 4)
  assert (kinds == expected)
  assert (leafDistance overlay (tA0 tiles) (tA1 tiles) == Just 1)
  assert (leafDistance overlay (tA1 tiles) (tA25 tiles) == Just 24)
  assert (leafDistance overlay (tA0 tiles) (tB1 tiles) == Nothing)
  assert (Map.lookup (tA0 tiles) (leafTerminalAdjacency overlay) == Just (Map.fromList [(tA1 tiles, 1), (tA25 tiles, 25)]))
  assert (Map.lookup (tA1 tiles) (leafTerminalAdjacency overlay) == Just (Map.fromList [(tA0 tiles, 1), (tA25 tiles, 24)]))
  assert (all (symmetric (leafTerminalAdjacency overlay)) (Map.toList (leafTerminalAdjacency overlay)))
  assert (all (\(source, neighbours) -> not (Map.member source neighbours)) (Map.toList (leafTerminalAdjacency overlay)))
  assert (isWalkable (worldCollision world) (tD0 tiles))
  assert (tS1 tiles `elem` walkingNeighborsRaw world (tS0 tiles))
  assert (tS2 tiles `elem` walkingNeighborsRaw world (tS1 tiles))
  assert (Map.member (tS0 tiles) (hierarchySeparatorNodes hierarchy))
  assert (Map.member (tS2 tiles) (hierarchySeparatorNodes hierarchy))
  assert (Map.member (LeafId 1 "b") (leafOverlays hierarchy))
 where
  symmetric adjacency (source, neighbours) =
    all (\(target, distance) -> (Map.lookup target adjacency >>= Map.lookup source) == Just distance) (Map.toList neighbours)

data Case = Case String Query Expect
data Expect = Reachable | Unreachable

cases :: Tiles -> [Case]
cases t =
  [ Case "same tile" (query (tA0 t) (tA0 t) Set.empty True) Reachable
  , Case "same leaf direct" (query (tA3 t) (tA8 t) Set.empty False) Reachable
  , Case "separator start target" (query (tS0 t) (tS2 t) Set.empty False) Reachable
  , Case "cross-region walking" (query (tA0 t) (tB1 t) Set.empty False) Reachable
  , Case "directed local transport" (query (tB1 t) (tC0 t) (Set.singleton "SYNTHETIC_BOAT") False) Reachable
  , Case "global teleport" (query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_GLOBAL") False) Reachable
  , Case "missing inventory blocks global teleport" ((query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_GLOBAL") False) {inventoryItems = Map.empty}) Unreachable
  , Case "default bank supplies missing global item" ((query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True) {inventoryItems = Map.empty}) Reachable
  , Case "custom bank blocks missing global item" ((query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True) {inventoryItems = Map.empty, bankItems = BankItems Map.empty}) Unreachable
  , Case "blocked transport origin attachment" (query (tC0 t) (tE0 t) (Set.singleton "SYNTHETIC_RING") False) Reachable
  , Case "blocked transport destination exit" (walkingQuery (tE0 t) (tC0 t)) Reachable
  , Case "banking enabled" (query (tA0 t) (tA3 t) Set.empty True) Reachable
  , Case "leaf re-entry" (query (tA0 t) (tA25 t) (Set.fromList ["SYNTHETIC_BOAT", "SYNTHETIC_RETURN"]) False) Reachable
  , Case "unreachable by walking" (walkingQuery (tA0 t) (tD0 t)) Unreachable
  ]

query :: Tile -> Tile -> Set.Set String -> Bool -> Query
query start target enabled bank =
  (defaultQuery start target)
    { enabledTransportTypes = enabled
    , bankPathEnabled = bank
    }

walkingQuery :: Tile -> Tile -> Query
walkingQuery start target = (query start target Set.empty False) { allowTransports = False }

checkRoute :: RawDijkstra -> TileAStar -> Hierarchical -> World -> Case -> IO ()
checkRoute raw tileAStar hierarchical world (Case name q expectation) = do
  let flat = findRoute raw q
      tile = findRoute tileAStar q
      abstract = findRoute hierarchical q
  case expectation of
    Reachable -> do
      assert (routeCost flat < maxBound)
      assert (routeCost tile == routeCost flat)
      assert (routeCost abstract == routeCost flat)
      assert (concreteCost world q (routeSteps tile) == routeCost tile)
      assert (concreteCost world q (routeSteps abstract) == routeCost abstract)
    Unreachable -> do
      assert (routeCost flat == maxBound)
      assert (routeCost tile == maxBound)
      assert (routeCost abstract == maxBound)
      assert (null (routeSteps tile))
      assert (null (routeSteps abstract))
  putStrLn (name <> ": " <> show (routeCost flat) <> " / " <> show (routeCost tile) <> " / " <> show (routeCost abstract))

checkReversePathDebug :: TileAStar -> Tiles -> IO ()
checkReversePathDebug tileAStar tiles = do
  let initialQuery = query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_GLOBAL") False
      initialDebug = reversePathDebug tileAStar initialQuery
  assertMsg ("initial global leaked: " <> show initialDebug) (all ((/= "SYNTHETIC_GLOBAL") . reverseEdgeLabel) (concatMap reverseStatePath (reverseDebugStates initialDebug)))
  let decline = reversePathDebug tileAStar ((query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_LOCAL_AT_BANK") True) {inventoryItems = Map.empty})
      (declineUnbanked, declineBanked) = twoStates decline
  assertMsg ("decline unbanked: " <> show declineUnbanked) (map reverseEdgeType (reverseStatePath declineUnbanked) == ["bank", "transport"])
  assertMsg ("decline transitions: " <> show (reverseStatePath declineUnbanked)) (map transition (reverseStatePath declineUnbanked) == [(False, True), (True, True)])
  assertMsg ("decline banked: " <> show declineBanked) (map reverseEdgeType (reverseStatePath declineBanked) == ["transport"])
  assert (map transition (reverseStatePath declineBanked) == [(True, True)])
  let mixed = reversePathDebug tileAStar ((query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True) {inventoryItems = Map.empty})
      (mixedUnbanked, mixedBanked) = twoStates mixed
  assertMsg ("mixed unbanked value: " <> show mixedUnbanked) (reverseStateDistance mixedUnbanked == reverseStateHeuristic mixedUnbanked)
  assertMsg ("mixed banked: " <> show mixedBanked) (reverseStateUnreachable mixedBanked)
  assertMsg ("mixed unbanked path: " <> show (reverseStatePath mixedUnbanked)) (map reverseEdgeType (reverseStatePath mixedUnbanked) == ["transport"])
  assertMsg ("mixed transitions: " <> show (reverseStatePath mixedUnbanked)) (map transition (reverseStatePath mixedUnbanked) == [(False, True)])
  let oneMissing = reversePathDebug tileAStar ((query (tE0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_LOCAL") False) {inventoryItems = Map.empty})
      (missingUnbanked, missingBanked) = twoStates oneMissing
  assertMsg ("missing unbanked: " <> show missingUnbanked) (reverseStateUnreachable missingUnbanked)
  assertMsg ("missing banked: " <> show missingBanked) (not (reverseStateUnreachable missingBanked))
  assert (map reverseEdgeType (reverseStatePath missingBanked) == ["transport"])
 where
  transition edge = (reverseEdgeFromBanked edge, reverseEdgeToBanked edge)
  twoStates debug =
    case reverseDebugStates debug of
      [unbanked, banked] -> (unbanked, banked)
      states -> error ("expected two reverse debug states, got " <> show (length states))

concreteCost :: World -> Query -> [RouteStep] -> Int
concreteCost world q = snd . foldl step (queryStart q, 0)
 where
  step (current, total) routeStep =
    case routeStep of
      Walk next
        | next == current -> (next, total)
        | next `elem` walkingNeighborsRaw world current -> (next, total + 1)
        | otherwise -> error ("illegal reconstructed walk: " <> coordinateText current <> " -> " <> coordinateText next)
      UseTransport name next ->
        let costs = [duration t | t <- allTransports world, label t == name, destination t == Just next]
         in (next, total + must "transport step" (listHead costs))
  label transport = if null (displayInfo transport) then transportType transport else displayInfo transport

allTransports :: World -> [Transport]
allTransports world = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world

listHead :: [a] -> Maybe a
listHead [] = Nothing
listHead (x:_) = Just x

must :: String -> Maybe a -> a
must name = maybe (error name) id

assert :: Bool -> IO ()
assert True = pure ()
assert False = fail "synthetic hierarchy assertion failed"

assertMsg :: String -> Bool -> IO ()
assertMsg _ True = pure ()
assertMsg message False = fail message
