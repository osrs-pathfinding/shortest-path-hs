module Main (main) where

import Data.Aeson (eitherDecode, encode)
import Data.Bits (setBit)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.ReferenceDijkstra
import ShortestPath.Pathfinder
import ShortestPath.Separator
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.World

main :: IO ()
main = do
  let eligible = separatorCandidates 20 (IntSet.fromList [1, 3])
        [(1, [1 :: Int .. 21]), (2, [1 .. 100]), (3, [1 .. 20])]
      acceptanceConfig = SeparatorConfig 20 5 2 40 "strong" 42
  check "only large reachable components are eligible" (map fst eligible == [1])
  check "narrow separator accepted" (separatorRejection acceptanceConfig 10 10 2 == Nothing)
  check "small child rejected" (separatorRejection acceptanceConfig 4 10 1 /= Nothing)
  check "wide separator rejected" (separatorRejection acceptanceConfig 10 10 3 /= Nothing)
  let base = World (collisionMap tiles) Map.empty [] Set.empty Nothing
      identity = walkingTopologyIdentity base
      emptyArtifact = artifact identity []
      cuts = Set.toAscList (Set.fromList
        [ canonicalCut from to
        | from <- collisionTiles (worldCollision base)
        , to <- walkingNeighbors base from
        , let (fx, _, _) = unpackTile from
        , let (tx, _, _) = unpackTile to
        , fx <= 17
        , tx >= 18
        ])
      splitArtifact = artifact identity cuts
  decoded <- either fail pure (eitherDecode (encode splitArtifact))
  check "serialization" (decoded == splitArtifact)
  unsplit <- build (base {worldSeparatorArtifact = Just emptyArtifact})
  split <- build (base {worldSeparatorArtifact = Just decoded})
  let unsplitTopology = tileTopology unsplit
      splitTopology = tileTopology split
  check "one natural component" (Vector.length (componentIds (topologyNaturalComponents splitTopology)) == 1)
  check "empty artifact compatibility" (Vector.length (componentIds (topologyRoutingComponents unsplitTopology)) == 1)
  check "split into two" (Vector.length (componentIds (topologyRoutingComponents splitTopology)) == 2)
  check "several separator edges" (length cuts >= 2)
  check "all crossings retained" (length (topologySeparatorCrossings splitTopology) == length cuts)
  assertWalkingInvariant splitTopology
  let start = packTile 11 11 0
      target = packTile 24 11 0
      unsplitRoute = findRouteTileAStar unsplit (defaultQuery start target)
      splitRoute = findRouteTileAStar split (defaultQuery start target)
      referenceRoute = findRouteReferenceDijkstra (ReferenceDijkstra splitTopology) (defaultQuery start target)
  check "route cost preserved" (routeCost splitRoute == routeCost unsplitRoute)
  check "reference route cost" (routeCost splitRoute == routeCost referenceRoute)
  checkDisconnected identity
  case worldTopologyFromComponents policy base (topologyNaturalComponents splitTopology) of
    Left MissingSeparatorArtifact -> pure ()
    other -> fail ("missing separator artifact accepted: " <> showEither other)
  putStrLn "separator topology: pass"
 where
  tiles = [packTile x y 0 | x <- [10 .. 16] <> [18 .. 24], y <- [10 .. 12]]
    <> [packTile 17 y 0 | y <- [10 .. 12]]
  policy = StructuralReachabilityPolicy [packTile 11 11 0] Set.empty
  artifact identity cuts = SeparatorArtifact separatorArtifactVersion identity (SeparatorConfig 20 2 2 20 "strong" 42) cuts
  build world = either (fail . show) buildTileAStarFromTopology =<< buildWorldTopologyWithPolicy policy world
  showEither (Left err) = show err
  showEither (Right _) = "Right topology"

assertWalkingInvariant :: WorldTopology -> IO ()
assertWalkingInvariant topology = mapM_ checkEdge
  [ (from, to)
  | from <- collisionTiles (worldCollision (topologyWorld topology))
  , to <- walkingNeighbors (topologyWorld topology) from
  , from < to
  ]
 where
  crossingSet = Set.fromList [canonicalCut (crossingFromTile edge) (crossingToTile edge) | edge <- topologySeparatorCrossings topology]
  checkEdge (from, to) = do
    let same = routingComponentOfTile topology from == routingComponentOfTile topology to
        explicit = Set.member (canonicalCut from to) crossingSet
    check "walking edge representation" (same /= explicit)

checkDisconnected :: String -> IO ()
checkDisconnected _ = do
  let isolated = packTile 40 40 0
      base = World (collisionMap ([packTile x 10 0 | x <- [10 .. 12]] <> [isolated])) Map.empty [] Set.empty Nothing
      world = withEmptySeparatorArtifact base
      policy = StructuralReachabilityPolicy [packTile 10 10 0] Set.empty
  topology <- either (fail . show) pure =<< buildWorldTopologyWithPolicy policy world
  check "disconnected components unchanged" (Vector.length (componentIds (topologyNaturalComponents topology)) == Vector.length (componentIds (topologyRoutingComponents topology)))

collisionMap :: [Tile] -> CollisionMap
collisionMap values = CollisionMap (Map.fromList [(region, bytes region) | region <- Set.toList (Set.fromList (map tileRegion values))])
 where
  tileRegion tile = let (x, y, _) = unpackTile tile in (x `div` 64, y `div` 64)
  bytes region = BL.pack [byteAt region ix | ix <- [0 .. 8191]]
  byteAt region ix = foldr set 0 [bit | tile <- values, tileRegion tile == region, bit <- collisionBits tile, bit `div` 8 == ix]
  set bit value = setBit value (bit `mod` 8)
  collisionBits tile = [base, base + 1]
   where
    (x, y, plane) = unpackTile tile
    base = ((plane * 4096 + (y `mod` 64) * 64 + (x `mod` 64)) * 2)

check :: String -> Bool -> IO ()
check _ True = pure ()
check label False = fail label
