module ShortestPath.Internal.RgbaTile
  ( rgbaTile
  ) where

import qualified Data.Vector.Storable as Storable
import Data.Word (Word8)
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "spm_rgba_tile"
  c_rgbaTile :: Int -> Ptr Int -> Ptr Int -> Ptr Int -> Int -> Int -> Int -> Int -> Int -> Ptr Word8 -> IO ()

-- | Render equally sized borrowed x/y/value vectors into a caller-owned
-- 256x256 RGBA buffer. The C call does not retain any pointer.
rgbaTile :: Storable.Vector Int -> Storable.Vector Int -> Storable.Vector Int -> Int -> Int -> Int -> Int -> Int -> Ptr Word8 -> IO ()
rgbaTile xs ys values tx ty minimumValue maximumValue bankLayer pixels
  | Storable.length xs /= count || Storable.length ys /= count =
      fail "rgba tile coordinate/value length mismatch"
  | otherwise =
      Storable.unsafeWith xs $ \xPtr ->
        Storable.unsafeWith ys $ \yPtr ->
          Storable.unsafeWith values $ \valuePtr ->
            c_rgbaTile count xPtr yPtr valuePtr tx ty minimumValue maximumValue bankLayer pixels
 where
  count = Storable.length values
