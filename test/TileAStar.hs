module Main (main) where

import Data.Bits (setBit)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Exact.TileAStar
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  checkSeasonalReachability
  mapM_ check cases
  mapM_ checkSparse sparseCases
  putStrLn "tile astar transform/sparse walking: pass"

checkSeasonalReachability :: IO ()
checkSeasonalReachability = do
  astar <- buildTileAStar seasonalWorld
  assert (length [() | (_, _, reachable, _, _, _, _, _, _) <- componentFacts astar, reachable] == 2)
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
