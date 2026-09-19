module Main (main) where

import Data.Bits (setBit)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word8)

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.StaticArtifact
import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Separator
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  checkGolden
  checkSyntheticRoundTrip
  checkRealRoundTripAndAttachments
  putStrLn "routing static artifact: pass"

checkGolden :: IO ()
checkGolden = do
  encoded <- either fail pure (encodeRoutingStaticV1 goldenValue)
  assert (encoded == goldenBytes) "golden bytes differ"
  assert (decodeRoutingStaticV1 goldenBytes == Right goldenValue) "golden decode differs"
  case decodeRoutingStaticV1 (goldenBytes <> BL.singleton 0) of
    Left _ -> pure ()
    Right _ -> fail "trailing bytes were accepted"

checkSyntheticRoundTrip :: IO ()
checkSyntheticRoundTrip = do
  encoded <- either fail pure (encodeRoutingStaticV1 goldenValue)
  decoded <- either fail pure (decodeRoutingStaticV1 encoded)
  assert (decoded == goldenValue) "synthetic decode/encode round trip differs"
  reencoded <- either fail pure (encodeRoutingStaticV1 decoded)
  assert (reencoded == encoded) "synthetic encode/decode/encode is not deterministic"
  let network = SparseWalkingNetwork
        (fromIntegral (artifactSparseOriginalCount decoded))
        (fromIntegral (artifactSparseVertexCount decoded))
        (fromIntegral (artifactSparseSteinerCount decoded))
        (fromIntegral (artifactSparseUndirectedEdgeCount decoded))
        (Vector.map fromIntegral (artifactSparseOffsets decoded))
        (Vector.map fromIntegral (artifactSparseDestinations decoded))
        (Vector.map fromIntegral (artifactSparseWeights decoded))
  assert (sparseWalkingDistance network 0 1 == Just 4) "round-tripped sparse network has the wrong distance"

checkRealRoundTripAndAttachments :: IO ()
checkRealRoundTripAndAttachments = do
  let root = packTile 100 100 0
      blocked = packTile 101 100 0
      destination = packTile 300 300 0
      globalDestination = packTile 500 500 0
      local = transport "LOCAL" (Just blocked) (Just destination)
      global = transport "GLOBAL" Nothing (Just globalDestination)
      world = withEmptySeparatorArtifact (World
        (collisionMap [root, destination, globalDestination])
        (Map.singleton blocked [local])
        [global]
        (Set.singleton root)
        Nothing)
  astar <- either (fail . show) pure =<< buildTileAStarWithPolicy (StructuralReachabilityPolicy [root] Set.empty) world
  artifact <- either fail pure (routingStaticV1 astar)
  encoded <- either fail pure (encodeRoutingStaticV1 artifact)
  decoded <- either fail pure (decodeRoutingStaticV1 encoded)
  assert (decoded == artifact) "real artifact round trip differs"
  assert (Vector.length (artifactReachableBankTiles decoded) == 1) "reachable bank was not exported"
  let endpoints = [blocked, destination, globalDestination]
      actual point = routingStaticPointAttachments decoded (walkingNeighbors (topologyWorld (tileTopology astar))) point
      expected point = routingPointAttachments (tileTopology astar) point
      sites = map (Tile . fromIntegral) (Vector.toList (artifactSiteTiles decoded))
  mapM_ (\point -> assert (actual point == expected point) ("attachment mismatch at " <> coordinateText point)) (endpoints <> sites)
  checkSeparatorMapping
 where
  checkSeparatorMapping = do
    let from = packTile 10 10 0
        to = packTile 11 10 0
        crossingWorld = World
          (collisionMap [from, to])
          Map.empty
          []
          Set.empty
          (Just (SeparatorArtifact separatorArtifactVersion (walkingTopologyIdentity crossingWorld)
            (SeparatorConfig 0 0 0 0 "test" 0) [canonicalCut from to]))
    crossingAstar <- either (fail . show) pure =<< buildTileAStarWithPolicy (StructuralReachabilityPolicy [from] Set.empty) crossingWorld
    crossingArtifact <- either fail pure (routingStaticV1 crossingAstar)
    crossingEncoded <- either fail pure (encodeRoutingStaticV1 crossingArtifact)
    crossingDecoded <- either fail pure (decodeRoutingStaticV1 crossingEncoded)
    assert (Vector.length (artifactCrossingFromSite crossingDecoded) == 1) "separator crossing was not exported"
    let fromSite = fromIntegral (artifactCrossingFromSite crossingDecoded Vector.! 0)
        toSite = fromIntegral (artifactCrossingToSite crossingDecoded Vector.! 0)
        siteTiles = artifactSiteTiles crossingDecoded
        actual point = routingStaticPointAttachments crossingDecoded (walkingNeighbors (topologyWorld (tileTopology crossingAstar))) point
        expected point = routingPointAttachments (tileTopology crossingAstar) point
    assert (siteTiles Vector.! fromSite == fromIntegral (unTile from)) "separator source site was remapped"
    assert (siteTiles Vector.! toSite == fromIntegral (unTile to)) "separator destination site was remapped"
    assert (actual from == expected from && actual to == expected to) "separator endpoint attachments changed"

goldenValue :: RoutingStaticV1
goldenValue = RoutingStaticV1
  (Vector.fromList [0x00000002, 0xC0010001])
  (Vector.fromList [0, 1])
  (Vector.fromList [0x05, 0x80])
  (Vector.fromList [-1, 1])
  (Vector.fromList [1, -1])
  (Vector.fromList [0x00000002, 0xC0010001])
  (Vector.fromList [0, 1, 1])
  (Vector.fromList [0])
  2
  (Vector.fromList [0, 1, 1])
  (Vector.fromList [0])
  (Vector.fromList [0xC0010001])
  (Vector.fromList [0])
  (Vector.fromList [1])
  (Vector.fromList [7])
  2 3 1 1 2
  (Vector.fromList [0, 1, 2, 2])
  (Vector.fromList [1, 0])
  (Vector.fromList [4, 4])

goldenBytes :: BL.ByteString
goldenBytes = BL.pack (concat
  [ ascii "RSPWORLD"
  , u32 1, u32 0
  , u32 2, words32 [0x00000002, 0xC0010001], ints32 [0, 1], [5, 128], ints32 [-1, 1], ints32 [1, -1]
  , u32 2, words32 [0x00000002, 0xC0010001]
  , u32 1, words32 [0, 1, 1], ints32 [0]
  , u32 2, u32 1, words32 [0, 1, 1], ints32 [0]
  , u32 1, words32 [0xC0010001]
  , u32 1, ints32 [0], ints32 [1], ints32 [7]
  , words32 [2, 3, 1, 1, 2]
  , words32 [0, 1, 2, 2], ints32 [1, 0], ints32 [4, 4]
  ])
 where
  ascii :: String -> [Word8]
  ascii = map (fromIntegral . fromEnum)
  words32 :: [Integer] -> [Word8]
  words32 = concatMap le32
  ints32 :: [Integer] -> [Word8]
  ints32 = concatMap le32
  u32 :: Integer -> [Word8]
  u32 = le32
  le32 :: Integer -> [Word8]
  le32 value = [ fromIntegral (value `mod` 256)
              , fromIntegral ((value `div` 256) `mod` 256)
              , fromIntegral ((value `div` 65536) `mod` 256)
              , fromIntegral ((value `div` 16777216) `mod` 256)
              ]

transport :: String -> Maybe Tile -> Maybe Tile -> Transport
transport kind from to = Transport kind from to 1 kind "" False Nothing [] Nothing [] [] [] "synthetic"

collisionMap :: [Tile] -> CollisionMap
collisionMap tiles = CollisionMap (Map.fromList [(region, bytes region) | region <- Set.toList (Set.fromList (map tileRegion tiles))])
 where
  tileRegion tile = let (x, y, _) = unpackTile tile in (x `div` 64, y `div` 64)
  bytes region = BL.pack [byteAt region ix | ix <- [0 .. 8191]]
  byteAt region ix = foldr set 0 [bit | tile <- tiles, tileRegion tile == region, let base = collisionBit tile, bit <- [base, base + 1], bit `div` 8 == ix]
  set bit value = setBit value (bit `mod` 8)
  collisionBit tile = ((plane * 4096 + (y `mod` 64) * 64 + (x `mod` 64)) * 2)
   where (x, y, plane) = unpackTile tile

assert :: Bool -> String -> IO ()
assert True _ = pure ()
assert False message = fail message
