module Main (main) where

import Data.Bits (setBit)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.ReferenceDijkstra
import ShortestPath.Account
import ShortestPath.Pathfinder
import ShortestPath.Requirements
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  let (world, tiles) = synthetic
      defaults = defaultQuery (tA0 tiles) (tA1 tiles)
  assert (not (Set.member "SEASONAL_TRANSPORTS" (enabledTransportTypes defaults)))
  assert (Set.member "TELEPORTATION_ITEM" (enabledTransportTypes defaults))
  tileAStar <- mustRight =<< buildTileAStarWithPolicy (syntheticPolicy (tA0 tiles)) world
  let reference = ReferenceDijkstra (tileTopology tileAStar)
  mapM_ (checkRoute reference tileAStar world) (cases tiles)
  let globalQuery = query (tA3 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_GLOBAL") False
      rawGlobalRoute = findRoute reference globalQuery
      tileGlobalRoute = findRoute tileAStar globalQuery
  assert (routeSteps rawGlobalRoute == [UseTransport "SYNTHETIC_GLOBAL" (tD1 tiles)])
  assert (routeSteps tileGlobalRoute == [UseTransport "SYNTHETIC_GLOBAL" (tD1 tiles)])
  checkReversePathDebug tileAStar tiles
  checkTransportOnlyEndpoint reference tileAStar tiles
  checkIntermediateTransportEndpoint reference tileAStar tiles
  checkHeuristicPruning tileAStar tiles
  checkMultiplePointAttachments

checkMultiplePointAttachments :: IO ()
checkMultiplePointAttachments = do
  let left = packTile 10 10 0
      point = packTile 11 10 0
      right = packTile 12 10 0
      dead = packTile 20 20 0
      bridge = local "SYNTHETIC_SHARED_POINT" point dead 1
      shared = local "SYNTHETIC_SHARED_POINT_2" point left 1
      world = World (collisionMap [left, right]) (Map.singleton point [bridge, shared]) [] Set.empty
      routeQuery = query left right (Set.singleton "SYNTHETIC_SHARED_POINT") False
  tileAStar <- mustRight =<< buildTileAStarWithPolicy (syntheticPolicy left) world
  let reference = ReferenceDijkstra (tileTopology tileAStar)
      attachments = [cid | (_, Just cid, _) <- pointAccessFacts tileAStar point]
      referenceRoute = findRoute reference routeQuery
      tileRoute = findRoute tileAStar routeQuery
  assertMsg ("attachments: " <> show attachments) (length attachments == 2)
  assertMsg ("reference route: " <> show referenceRoute) (routeCost referenceRoute == 2)
  assertMsg ("tile route: " <> show tileRoute) (routeCost tileRoute == 2)
  assertMsg "reverse shared-point route" (routeCost (findRoute tileAStar (query right left (Set.singleton "SYNTHETIC_SHARED_POINT") False)) == 2)

collisionMap :: [Tile] -> CollisionMap
collisionMap tiles = CollisionMap (Map.fromList [(region, bytes region) | region <- Set.toList (Set.fromList (map tileRegion tiles))])
 where
  tileRegion tile = let (x, y, _) = unpackTile tile in (x `div` 64, y `div` 64)
  bytes region = BL.pack [byteAt region ix | ix <- [0 .. 8191]]
  byteAt region ix = foldr set 0 [bit | tile <- tiles, tileRegion tile == region, let bit = collisionBit tile, bit `div` 8 == ix]
  set bit value = setBit value (bit `mod` 8)
  collisionBit tile = ((plane * 4096 + (y `mod` 64) * 64 + (x `mod` 64)) * 2)
   where (x, y, plane) = unpackTile tile

syntheticPolicy :: Tile -> StructuralReachabilityPolicy
syntheticPolicy seed = StructuralReachabilityPolicy [seed] Set.empty

mustRight :: Show e => Either e a -> IO a
mustRight = either (fail . show) pure

synthetic :: (World, Tiles)
synthetic =
  ( World (CollisionMap (Map.singleton (1, 1) collisionBytes)) transports globals banks
  , Tiles a0 a1 a3 a8 a25 b1 c0 d0 d1 e0 s0 s2 unknown xSite ySite
  )
 where
  a0 = packTile 100 100 0
  a1 = packTile 101 100 0
  a3 = packTile 103 100 0
  a8 = packTile 108 100 0
  a25 = packTile 125 100 0
  s0 = packTile 100 99 0
  s1 = packTile 101 99 0
  s2 = packTile 102 99 0
  b0 = packTile 103 99 0
  b1 = packTile 104 99 0
  c0 = packTile 110 110 0
  c1 = packTile 111 110 0
  d0 = packTile 110 111 0
  d1 = packTile 111 111 0
  e0 = packTile 109 110 0
  unknown = packTile 200 200 0
  xSite = packTile 210 210 0
  ySite = packTile 220 220 0
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
  transports = Map.fromListWith (<>)
    [ (a0, [ local "SYNTHETIC_DIRECT" a0 a1 10
           , local "SYNTHETIC_LONG" a0 d1 20
           , localReq "SYNTHETIC_BANK_LOCAL_AT_BANK" a0 d1 5 (ItemOne (ItemTerm "999" 1))
           , local "SYNTHETIC_UNKNOWN" a0 unknown 1
           , local "SYNTHETIC_X_1" a0 xSite 5
           , local "SYNTHETIC_DEAD_END" a0 ySite 1
           ])
    , (xSite, [local "SYNTHETIC_X_2" xSite c0 7])
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
  , tS0 :: Tile, tS2 :: Tile, tUnknown :: Tile, tX :: Tile, tY :: Tile
  }

local :: String -> Tile -> Tile -> Int -> Transport
local kind from to cost = Transport kind (Just from) (Just to) cost kind "" False Nothing [] Nothing [] [] [] "synthetic"

localReq :: String -> Tile -> Tile -> Int -> ItemExpr -> Transport
localReq kind from to cost itemReq = Transport kind (Just from) (Just to) cost kind "" False Nothing [] (Just itemReq) [] [] [] "synthetic"

global :: String -> Tile -> Int -> Maybe ItemExpr -> Transport
global kind to cost itemReq = Transport kind Nothing (Just to) cost kind "" False Nothing [] itemReq [] [] [] "synthetic"

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
  , Case "missing inventory blocks global teleport" (withoutInventory (query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_GLOBAL") False)) Unreachable
  , Case "default bank supplies missing global item" (withoutInventory (query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True)) Reachable
  , Case "custom bank blocks missing global item" (withoutBank (withoutInventory (query (tA0 t) (tD1 t) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True))) Unreachable
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
    , requirementMode = ConfiguredRequirements syntheticAccount
    }

syntheticAccount :: AccountState
syntheticAccount = emptyAccountState { accountInventory = Map.singleton "13393" 1, accountBank = Map.singleton "999" 1 }

withoutInventory :: Query -> Query
withoutInventory q = q { requirementMode = ConfiguredRequirements (syntheticAccount { accountInventory = Map.empty }) }

withoutBank :: Query -> Query
withoutBank q = q { requirementMode = ConfiguredRequirements (syntheticAccount { accountInventory = Map.empty, accountBank = Map.empty }) }

walkingQuery :: Tile -> Tile -> Query
walkingQuery start target = (query start target Set.empty False) { allowTransports = False }

checkRoute :: ReferenceDijkstra -> TileAStar -> World -> Case -> IO ()
checkRoute raw tileAStar world (Case name q expectation) = do
  let flat = findRoute raw q
      tile = findRoute tileAStar q
  case expectation of
    Reachable -> do
      assert (routeCost flat < maxBound)
      assert (routeCost tile == routeCost flat)
      assert (concreteCost world q (routeSteps tile) == routeCost tile)
    Unreachable -> do
      assert (routeCost flat == maxBound)
      assert (routeCost tile == maxBound)
      assert (null (routeSteps tile))
  putStrLn (name <> ": " <> show (routeCost flat) <> " / " <> show (routeCost tile))

checkReversePathDebug :: TileAStar -> Tiles -> IO ()
checkReversePathDebug tileAStar tiles = do
  let initialQuery = query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_GLOBAL") False
      initialDebug = reversePathDebug tileAStar initialQuery
  assertMsg ("initial global leaked: " <> show initialDebug) (all ((/= "SYNTHETIC_GLOBAL") . reverseEdgeLabel) (concatMap reverseStatePath (reverseDebugStates initialDebug)))
  let decline = reversePathDebug tileAStar (withoutInventory (query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_LOCAL_AT_BANK") True))
      (declineUnbanked, declineBanked) = twoStates decline
  assertMsg ("decline unbanked: " <> show declineUnbanked) (map reverseEdgeType (reverseStatePath declineUnbanked) == ["bank", "transport"])
  assertMsg ("decline transitions: " <> show (reverseStatePath declineUnbanked)) (map transition (reverseStatePath declineUnbanked) == [(False, True), (True, True)])
  assertMsg ("decline banked: " <> show declineBanked) (map reverseEdgeType (reverseStatePath declineBanked) == ["transport"])
  assert (map transition (reverseStatePath declineBanked) == [(True, True)])
  let mixed = reversePathDebug tileAStar (withoutInventory (query (tA0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_GLOBAL") True))
      (mixedUnbanked, mixedBanked) = twoStates mixed
  assertMsg ("mixed unbanked value: " <> show mixedUnbanked) (reverseStateDistance mixedUnbanked == reverseStateHeuristic mixedUnbanked)
  assertMsg ("mixed banked: " <> show mixedBanked) (reverseStateUnreachable mixedBanked)
  assertMsg ("mixed unbanked path: " <> show (reverseStatePath mixedUnbanked)) (map reverseEdgeType (reverseStatePath mixedUnbanked) == ["transport"])
  assertMsg ("mixed transitions: " <> show (reverseStatePath mixedUnbanked)) (map transition (reverseStatePath mixedUnbanked) == [(False, True)])
  let oneMissing = reversePathDebug tileAStar (withoutInventory (query (tE0 tiles) (tD1 tiles) (Set.singleton "SYNTHETIC_BANK_LOCAL") False))
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

checkHeuristicPruning :: TileAStar -> Tiles -> IO ()
checkHeuristicPruning tileAStar tiles = do
  let unknownQuery = (query (tA0 tiles) (tA25 tiles) (Set.singleton "SYNTHETIC_UNKNOWN") False)
      noSeedQuery = walkingQuery (tA0 tiles) (tD0 tiles)
  (unknownRoute, unknownTimings, _) <- findRouteProfiledTileAStarWithTrace False tileAStar unknownQuery
  (_, noSeedTimings, _) <- findRouteProfiledTileAStarWithTrace False tileAStar noSeedQuery
  let unknownCounters = tileSearchCounters unknownTimings
      noSeedCounters = tileSearchCounters noSeedTimings
  assert (routeCost unknownRoute == 25)
  assert (tileUnknownComponentPrunes unknownCounters > 0)
  assert (tileNoReverseSeedPrunes noSeedCounters > 0)

checkTransportOnlyEndpoint :: ReferenceDijkstra -> TileAStar -> Tiles -> IO ()
checkTransportOnlyEndpoint raw tileAStar tiles = do
  let targetQuery = query (tA0 tiles) (tUnknown tiles) (Set.singleton "SYNTHETIC_UNKNOWN") False
      rawRoute = findRoute raw targetQuery
      tileRoute = findRoute tileAStar targetQuery
      expected = [UseTransport "SYNTHETIC_UNKNOWN" (tUnknown tiles)]
  assert (routeCost rawRoute == 1)
  assert (routeCost tileRoute == 1)
  assert (routeSteps tileRoute == expected)

checkIntermediateTransportEndpoint :: ReferenceDijkstra -> TileAStar -> Tiles -> IO ()
checkIntermediateTransportEndpoint raw tileAStar tiles = do
  let enabled = Set.fromList ["SYNTHETIC_X_1", "SYNTHETIC_X_2", "SYNTHETIC_DEAD_END"]
      routeQuery = query (tA0 tiles) (tC0 tiles) enabled False
      rawRoute = findRoute raw routeQuery
      tileRoute = findRoute tileAStar routeQuery
      expected = [ UseTransport "SYNTHETIC_X_1" (tX tiles)
                 , UseTransport "SYNTHETIC_X_2" (tC0 tiles)
                 ]
      xDebug = reversePathDebug tileAStar (query (tX tiles) (tC0 tiles) enabled False)
      yDebug = reversePathDebug tileAStar (query (tY tiles) (tC0 tiles) enabled False)
  assert (routeCost rawRoute == 12)
  assert (routeSteps rawRoute == expected)
  assert (routeCost tileRoute == 12)
  assert (routeSteps tileRoute == expected)
  assert (reverseStateDistance (unbankedState xDebug) == 7)
  assert (reverseStateHeuristic (unbankedState xDebug) == 7)
  assert (reverseStateUnreachable (unbankedState yDebug))
 where
  unbankedState debug =
    case reverseDebugStates debug of
      [state, _] -> state
      states -> error ("expected two reverse states, got " <> show (length states))

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
assert False = fail "synthetic pathfinder assertion failed"

assertMsg :: String -> Bool -> IO ()
assertMsg _ True = pure ()
assertMsg message False = fail message
