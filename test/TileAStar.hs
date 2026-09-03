module Main (main) where

import ShortestPath.Exact.TileAStar
import ShortestPath.Tile

main :: IO ()
main = do
  mapM_ check cases
  putStrLn "tile astar transform: pass"
 where
  check (box, seeds) =
    assert (chebyshevTransform box seeds == chebyshevTransformSlow box seeds)
      >> assert (chebyshevTransformC box seeds == chebyshevTransformSlow box seeds)

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
