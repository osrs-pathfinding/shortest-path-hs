module Main (main) where

import Control.Monad.ST (runST)
import Data.Bits (setBit)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.SparseWalking
import ShortestPath.Internal.DistanceTransform
import ShortestPath.Internal.MutableHeap
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  checkHeapGrowth
  checkTopologySemantics
  checkSeasonalReachability
  mapM_ check cases
  mapM_ checkSparse sparseCases
  putStrLn "tile astar transform/sparse walking: pass"

checkHeapGrowth :: IO ()
checkHeapGrowth =
  assert (runST $ do
    heap <- heapNew 1
    mapM_ (\(priority, state, cost) -> heapPush heap priority state cost)
      [(2, 10, 1), (1, 30, 5), (1, 20, 5), (1, 15, 7)]
    sequence [heapPop heap, heapPop heap, heapPop heap, heapPop heap, heapPop heap]
      >>= pure . (== map Just [(1, 20, 5), (1, 30, 5), (1, 15, 7), (2, 10, 1)] <> [Nothing]))

checkTopologySemantics :: IO ()
checkTopologySemantics = do
  let a = packTile 10 10 0
      point = packTile 11 10 0
      b = packTile 12 10 0
      c = packTile 20 20 0
      zero = packTile 30 30 0
      transports = Map.fromList
        [ (point, [transport "SHARED_1" point zero, transport "SHARED_2" point a])
        , (a, [transport "A_TO_B" a b])
        ]
      world = World (collisionMap [a, b, c]) transports [] Set.empty
      policy = StructuralReachabilityPolicy [a] Set.empty
  topology <- either (fail . show) pure =<< buildWorldTopologyWithPolicy policy world
  let components = topologyNaturalComponents topology
      component tile = maybe (error "missing component") id (componentOfTile components tile)
      reachable = structurallyReachableIds (topologyStructuralReachability topology)
  assert (pointAttachments topology a == [component a])
  assert (Set.fromList (pointAttachments topology point) == Set.fromList [component a, component b])
  assert (pointAttachments topology zero == [])
  assert (IntSet.member (component a) reachable)
  assert (IntSet.member (component b) reachable)
  assert (IntSet.notMember (component c) reachable)
  missing <- buildWorldTopologyWithPolicy (StructuralReachabilityPolicy [zero] Set.empty) world
  case missing of
    Left (MissingStructuralReachabilitySeed tile) -> assert (tile == zero)
    Left err -> fail (show err)
    Right _ -> fail "missing structural seed was accepted"
  noSeeds <- buildWorldTopologyWithPolicy (StructuralReachabilityPolicy [] Set.empty) world
  case noSeeds of
    Left NoStructuralReachabilitySeeds -> pure ()
    _ -> fail "empty structural seed policy was accepted"
 where
  transport kind from to = Transport kind (Just from) (Just to) 1 kind "" False Nothing [] Nothing [] [] [] "synthetic"

checkSeasonalReachability :: IO ()
checkSeasonalReachability = do
  astar <- buildTileAStar seasonalWorld
  assert (length [() | (_, _, reachable, _, _, _, _, _, _) <- componentFacts (tileTopology astar), reachable] == 2)
 where
  root = packTile 3221 3218 0
  ordinaryTarget = packTile 100 100 0
  seasonalLocalTarget = packTile 200 200 0
  seasonalGlobalTarget = packTile 300 300 0
  seasonalWorld = World
    (collisionMap [root, ordinaryTarget, seasonalLocalTarget, seasonalGlobalTarget])
    (Map.fromList
      [ (root, [transport "ORDINARY" root ordinaryTarget])
      , (ordinaryTarget, [transport "SEASONAL_TRANSPORTS" ordinaryTarget seasonalLocalTarget])
      ])
    [globalTransport "SEASONAL_TRANSPORTS" seasonalGlobalTarget]
    Set.empty
  transport kind from to = Transport kind (Just from) (Just to) 1 kind "" False Nothing [] Nothing [] [] [] "synthetic"
  globalTransport kind to = Transport kind Nothing (Just to) 1 kind "" False Nothing [] Nothing [] [] [] "synthetic"

collisionMap :: [Tile] -> CollisionMap
collisionMap tiles = CollisionMap (Map.fromList [(region, bytes region) | region <- Set.toList (Set.fromList (map tileRegion tiles))])
 where
  tileRegion tile = let (x, y, _) = unpackTile tile in (x `div` 64, y `div` 64)
  bytes region = BL.pack [byteAt region ix | ix <- [0 .. 8191]]
  byteAt region ix = foldr set 0 [bit | tile <- tiles, tileRegion tile == region, let bit = collisionBit tile, bit `div` 8 == ix]
  set bit value = setBit value (bit `mod` 8)
  collisionBit tile = ((plane * 4096 + (y `mod` 64) * 64 + (x `mod` 64)) * 2)
   where (x, y, plane) = unpackTile tile

check :: (Box, [(Tile, Int)]) -> IO ()
check (box, seeds) =
  assert (chebyshevTransform box seeds == chebyshevTransformSlow box seeds)
    >> assert (chebyshevTransformC box seeds == chebyshevTransformSlow box seeds)

checkSparse :: [Tile] -> IO ()
checkSparse tiles = do
  let network = buildSparseWalkingNetwork (zip [0 ..] tiles)
  mapM_ (checkPair network tiles) [(a, b) | a <- [0 .. length tiles - 1], b <- [0 .. length tiles - 1]]

checkPair :: SparseWalkingNetwork -> [Tile] -> (Int, Int) -> IO ()
checkPair network tiles (a, b) =
  assert (sparseWalkingDistance network a b == Just (2 * cheb (tiles !! a) (tiles !! b)))

cases :: [(Box, [(Tile, Int)])]
cases =
  [ (box, take n (filter (inside box . fst) weightedSeeds))
  | w <- [0 .. 7]
  , h <- [0 .. 6]
  , let box = Box 0 0 w h 0
  , n <- [0 .. length weightedSeeds]
  ]
 where
  weightedSeeds =
    [ (packTile 0 0 0, 0)
    , (packTile 3 2 0, 5)
    , (packTile 7 6 0, 1)
    , (packTile 2 5 0, 9)
    ]
  inside box tile =
    let (x, y, p) = unpackTile tile
     in p == boxPlane box && x >= boxMinX box && x <= boxMaxX box && y >= boxMinY box && y <= boxMaxY box

assert :: Bool -> IO ()
assert True = pure ()
assert False = fail "tile astar assertion failed"

sparseCases :: [[Tile]]
sparseCases =
  [ []
  , [packTile 0 0 0]
  , [packTile 0 0 0, packTile 3 0 0]
  , [packTile 0 0 0, packTile 0 3 0]
  , [packTile 0 0 0, packTile 3 3 0]
  , [packTile 0 0 0, packTile 1 0 0, packTile 2 0 0, packTile 3 0 0]
  , [packTile 0 0 0, packTile 1 1 0, packTile 2 2 0, packTile 3 3 0]
  , [packTile 0 0 0, packTile 1 (-1) 0, packTile 2 (-2) 0, packTile 3 (-3) 0]
  , [packTile 0 0 0, packTile 1 1 0, packTile 2 0 0, packTile 3 1 0, packTile 4 0 0]
  , [packTile 5 1 0, packTile 4 2 0, packTile 3 3 0, packTile 2 4 0, packTile 1 5 0]
  , [packTile 0 0 0, packTile 0 0 0, packTile 2 1 0, packTile 2 1 0]
  , [packTile 10 0 0, packTile 0 10 0, packTile 9 1 0, packTile 1 9 0, packTile 5 5 0]
  ]

cheb :: Tile -> Tile -> Int
cheb a b =
  let (ax, ay, _) = unpackTile a
      (bx, by, _) = unpackTile b
   in max (abs (ax - bx)) (abs (ay - by))
