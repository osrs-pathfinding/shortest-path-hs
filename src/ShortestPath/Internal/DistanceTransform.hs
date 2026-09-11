module ShortestPath.Internal.DistanceTransform
  ( Box(..)
  , chebyshevTransform
  , chebyshevTransformC
  , chebyshevTransformSlow
  , componentBox
  , offset
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Foreign.ForeignPtr (mallocForeignPtrArray, withForeignPtr)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

import ShortestPath.Exact.TileAStar.RelaxedGraph (addCostDefault)
import ShortestPath.Tile

data Box = Box
  { boxMinX :: !Int
  , boxMinY :: !Int
  , boxMaxX :: !Int
  , boxMaxY :: !Int
  , boxPlane :: !Int
  }
  deriving stock (Eq, Show)

componentBox :: Vector.Vector Int -> Box
componentBox packedTiles
  | Vector.null packedTiles = Box 0 0 0 0 0
  | otherwise = Vector.foldl' expand (Box x y x y p) (Vector.tail packedTiles)
 where
  packed = Vector.head packedTiles
  (x, y, p) = unpackTile (Tile packed)
  expand box tilePacked =
    let (tx, ty, _) = unpackTile (Tile tilePacked)
     in box
          { boxMinX = min (boxMinX box) tx
          , boxMinY = min (boxMinY box) ty
          , boxMaxX = max (boxMaxX box) tx
          , boxMaxY = max (boxMaxY box) ty
          }

-- The C routine only reads the seed vectors and initializes every output cell.
-- The returned vector owns its ForeignPtr; equal inputs always produce equal output.
foreign import ccall unsafe "spm_chebyshev_transform"
  c_chebyshevTransform :: Int -> Int -> Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO ()

chebyshevTransform :: Box -> [(Tile, Int)] -> Vector.Vector Int
chebyshevTransform box seeds = runST $ do
  values <- Mutable.replicate size maxBound
  forM_ seeds $ \(tile, cost) ->
    case offset box tile of
      Nothing -> pure ()
      Just index -> do
        old <- Mutable.read values index
        when (cost < old) (Mutable.write values index cost)
  forwardRows values 0
  backwardRows values (height - 1)
  Vector.freeze values
 where
  width = boxMaxX box - boxMinX box + 1
  height = boxMaxY box - boxMinY box + 1
  size = width * height
  ix x y = y * width + x

  forwardRows :: Mutable.MVector s Int -> Int -> ST s ()
  forwardRows values y
    | y >= height = pure ()
    | otherwise = forwardColumns values y 0 >> forwardRows values (y + 1)

  forwardColumns :: Mutable.MVector s Int -> Int -> Int -> ST s ()
  forwardColumns values y x
    | x >= width = pure ()
    | otherwise = do
        let here = ix x y
        current <- Mutable.read values here
        best0 <- bestNeighbour values current (x - 1) (y - 1)
        best1 <- bestNeighbour values best0 x (y - 1)
        best2 <- bestNeighbour values best1 (x + 1) (y - 1)
        best3 <- bestNeighbour values best2 (x - 1) y
        when (best3 < current) (Mutable.write values here best3)
        forwardColumns values y (x + 1)

  backwardRows :: Mutable.MVector s Int -> Int -> ST s ()
  backwardRows values y
    | y < 0 = pure ()
    | otherwise = backwardColumns values y (width - 1) >> backwardRows values (y - 1)

  backwardColumns :: Mutable.MVector s Int -> Int -> Int -> ST s ()
  backwardColumns values y x
    | x < 0 = pure ()
    | otherwise = do
        let here = ix x y
        current <- Mutable.read values here
        best0 <- bestNeighbour values current (x + 1) y
        best1 <- bestNeighbour values best0 (x - 1) (y + 1)
        best2 <- bestNeighbour values best1 x (y + 1)
        best3 <- bestNeighbour values best2 (x + 1) (y + 1)
        when (best3 < current) (Mutable.write values here best3)
        backwardColumns values y (x - 1)

  bestNeighbour :: Mutable.MVector s Int -> Int -> Int -> Int -> ST s Int
  bestNeighbour values best x y
    | x < 0 || x >= width || y < 0 || y >= height = pure best
    | otherwise = do
        value <- Mutable.read values (ix x y)
        pure (min best (addCostDefault maxBound value 1))

chebyshevTransformC :: Box -> [(Tile, Int)] -> Vector.Vector Int
{-# NOINLINE chebyshevTransformC #-}
chebyshevTransformC box seeds =
  Generic.convert $
    unsafePerformIO $
      Storable.unsafeWith seedOffsets $ \offsetPtr ->
        Storable.unsafeWith seedCosts $ \costPtr ->
          do
            values <- mallocForeignPtrArray size
            withForeignPtr values $ \valuePtr ->
              c_chebyshevTransform width height (Storable.length seedOffsets) offsetPtr costPtr valuePtr
            pure (Storable.unsafeFromForeignPtr0 values size)
 where
  width = boxMaxX box - boxMinX box + 1
  height = boxMaxY box - boxMinY box + 1
  size = width * height
  validSeeds =
    [ (ix, cost)
    | (tile, cost) <- seeds
    , Just ix <- [offset box tile]
    ]
  seedOffsets = Storable.fromList (map fst validSeeds)
  seedCosts = Storable.fromList (map snd validSeeds)

chebyshevTransformSlow :: Box -> [(Tile, Int)] -> Vector.Vector Int
chebyshevTransformSlow box seeds = Vector.generate size valueAt
 where
  width = boxMaxX box - boxMinX box + 1
  height = boxMaxY box - boxMinY box + 1
  size = width * height
  valueAt ix =
    let (dy, dx) = ix `divMod` width
        tile = packTile (boxMinX box + dx) (boxMinY box + dy) (boxPlane box)
     in minimumDefault maxBound [addCostDefault maxBound cost distance | (seed, cost) <- seeds, Just distance <- [chebyshev2 tile seed]]

offset :: Box -> Tile -> Maybe Int
offset box tile =
  let (x, y, p) = unpackTile tile
      width = boxMaxX box - boxMinX box + 1
   in if p == boxPlane box && x >= boxMinX box && x <= boxMaxX box && y >= boxMinY box && y <= boxMaxY box
        then Just ((y - boxMinY box) * width + (x - boxMinX box))
        else Nothing

minimumDefault :: Ord a => a -> [a] -> a
minimumDefault fallback [] = fallback
minimumDefault _ values = minimum values
