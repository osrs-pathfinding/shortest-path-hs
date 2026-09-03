module Main (main) where

import ShortestPath.Exact.TileAStar
import ShortestPath.Tile

main :: IO ()
main = do
  mapM_ check cases
  mapM_ checkSparse sparseCases
  putStrLn "tile astar transform/sparse walking: pass"
 where
  check (box, seeds) =
    assert (chebyshevTransform box seeds == chebyshevTransformSlow box seeds)
      >> assert (chebyshevTransformC box seeds == chebyshevTransformSlow box seeds)
  checkSparse tiles = do
    let network = buildSparseWalkingNetwork (zip [0 ..] tiles)
    mapM_ (checkPair network tiles) [(a, b) | a <- [0 .. length tiles - 1], b <- [0 .. length tiles - 1]]
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
