{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module ShortestPath.Exact.TileAStar.HeuristicScan
  ( GeneratorScan
  , generatorScanFromVector
  , generatorScanLength
  , forceGeneratorScan
  , scanGenerators
  , scanGeneratorsScalar
  , scanGeneratorsSimd
  ) where

import Data.Array.Byte (ByteArray(..))
import Data.Bits ((.&.), shiftR)
import Data.Int (Int32)
import qualified Data.Vector.Primitive as Primitive
import qualified Data.Vector.Unboxed as Vector
import GHC.Exts
  ( Int(I#), Int#, ByteArray#, broadcastInt32X4#, indexInt32Array#
  , indexInt32ArrayAsInt32X4#, int32ToInt#, intToInt32#, isTrue#
  , negateInt32X4#, minusInt32X4#, plusInt32X4#, unpackInt32X4#
  , (+#), (-#), (<#), (>=#)
  )
import GHC.Internal.Prim (maxInt32X4#, minInt32X4#)

data GeneratorScan = GeneratorScan
  { generatorScanLength :: !Int
  , scalarFallback :: !(Vector.Vector (Int, Int))
  , generatorXs :: !(Primitive.Vector Int32)
  , generatorYs :: !(Primitive.Vector Int32)
  , generatorCosts :: !(Primitive.Vector Int32)
  , generatorSimdReady :: !Bool
  }

generatorScanFromVector :: Vector.Vector (Int, Int) -> GeneratorScan
generatorScanFromVector generators =
  GeneratorScan count generators xs ys costs simdReady
 where
  count = Vector.length generators
  simdReady = Vector.all (\(_, cost) -> cost >= 0 && cost <= maxSafeCost) generators
  xs = prepare (tileX . fst)
  ys = prepare (tileY . fst)
  costs = prepare snd
  prepare get
    | simdReady = Primitive.generate count (fromIntegral . get . Vector.unsafeIndex generators)
    | otherwise = Primitive.empty

scanGenerators :: GeneratorScan -> Int -> Int -> Int
{-# INLINE scanGenerators #-}
scanGenerators bucket x y
  | generatorScanLength bucket == 0 = maxBound
  | generatorScanLength bucket < 4 = scanGeneratorsScalar bucket x y
  | otherwise = scanGeneratorsSimd bucket x y

scanGeneratorsScalar :: GeneratorScan -> Int -> Int -> Int
{-# INLINE scanGeneratorsScalar #-}
scanGeneratorsScalar bucket x y
  | generatorScanLength bucket == 0 = maxBound
  | generatorSimdReady bucket = scanPreparedScalar bucket x y
  | otherwise = Vector.foldl' step maxBound (scalarFallback bucket)
 where
  step best (packed, cost) = min best (safeAdd cost (max (abs (x - tileX packed)) (abs (y - tileY packed))))

scanGeneratorsSimd :: GeneratorScan -> Int -> Int -> Int
{-# INLINE scanGeneratorsSimd #-}
scanGeneratorsSimd bucket@(GeneratorScan (I# count#) _ xs ys costs ready) (I# x#) (I# y#)
  | generatorScanLength bucket == 0 = maxBound
  | not ready = scanGeneratorsScalar bucket (I# x#) (I# y#)
  | otherwise =
      case (xs, ys, costs) of
        ( Primitive.Vector (I# xOffset#) _ (ByteArray xs#)
          , Primitive.Vector (I# yOffset#) _ (ByteArray ys#)
          , Primitive.Vector (I# costOffset#) _ (ByteArray costs#)
          ) -> I# (scanInt32X4# xs# xOffset# ys# yOffset# costs# costOffset# count# x# y#)

scanPreparedScalar :: GeneratorScan -> Int -> Int -> Int
{-# INLINE scanPreparedScalar #-}
scanPreparedScalar (GeneratorScan (I# count#) _ xs ys costs _) (I# x#) (I# y#) =
  case (xs, ys, costs) of
    ( Primitive.Vector (I# xOffset#) _ (ByteArray xs#)
      , Primitive.Vector (I# yOffset#) _ (ByteArray ys#)
      , Primitive.Vector (I# costOffset#) _ (ByteArray costs#)
      ) -> I# (scalarTail# xs# xOffset# ys# yOffset# costs# costOffset# 0# count# x# y# 2147483647#)

scanInt32X4# :: ByteArray# -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int#
  -> Int# -> Int# -> Int# -> Int#
{-# NOINLINE scanInt32X4# #-}
scanInt32X4# xs# xOffset# ys# yOffset# costs# costOffset# count# x# y# =
  case vectorLoop# 0# initial# of
    (# tailStart#, best# #) ->
      case unpackInt32X4# best# of
        (# a#, b#, c#, d# #) ->
          scalarTail# xs# xOffset# ys# yOffset# costs# costOffset# tailStart# count# x# y#
            (minInt# (int32ToInt# a#) (minInt# (int32ToInt# b#) (minInt# (int32ToInt# c#) (int32ToInt# d#))))
 where
  queryX# = broadcastInt32X4# (intToInt32# x#)
  queryY# = broadcastInt32X4# (intToInt32# y#)
  initial# = broadcastInt32X4# (intToInt32# 2147483647#)
  vectorLoop# index# best#
    | isTrue# (index# +# 4# >=# count# +# 1#) = (# index#, best# #)
    | otherwise =
        let sx# = indexInt32ArrayAsInt32X4# xs# (xOffset# +# index#)
            sy# = indexInt32ArrayAsInt32X4# ys# (yOffset# +# index#)
            dx0# = minusInt32X4# queryX# sx#
            dy0# = minusInt32X4# queryY# sy#
            dx# = maxInt32X4# dx0# (negateInt32X4# dx0#)
            dy# = maxInt32X4# dy0# (negateInt32X4# dy0#)
            distance# = maxInt32X4# dx# dy#
            reverseCosts# = indexInt32ArrayAsInt32X4# costs# (costOffset# +# index#)
            candidates# = plusInt32X4# reverseCosts# distance#
         in vectorLoop# (index# +# 4#) (minInt32X4# best# candidates#)

scalarTail# :: ByteArray# -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int#
  -> Int# -> Int# -> Int# -> Int# -> Int# -> Int#
{-# INLINE scalarTail# #-}
scalarTail# xs# xOffset# ys# yOffset# costs# costOffset# index# count# x# y# best#
  | isTrue# (index# >=# count#) = best#
  | otherwise =
      let sx# = int32ToInt# (indexInt32Array# xs# (xOffset# +# index#))
          sy# = int32ToInt# (indexInt32Array# ys# (yOffset# +# index#))
          cost# = int32ToInt# (indexInt32Array# costs# (costOffset# +# index#))
          dx# = absInt# (x# -# sx#)
          dy# = absInt# (y# -# sy#)
          candidate# = cost# +# maxInt# dx# dy#
       in scalarTail# xs# xOffset# ys# yOffset# costs# costOffset# (index# +# 1#) count# x# y#
            (minInt# best# candidate#)

forceGeneratorScan :: GeneratorScan -> Int
forceGeneratorScan bucket = generatorScanLength bucket
  + Primitive.length (generatorXs bucket)
  + Primitive.length (generatorYs bucket)
  + Primitive.length (generatorCosts bucket)

tileX, tileY :: Int -> Int
{-# INLINE tileX #-}
{-# INLINE tileY #-}
tileX packed = packed .&. 0x7fff
tileY packed = (packed `shiftR` 15) .&. 0x7fff

safeAdd :: Int -> Int -> Int
{-# INLINE safeAdd #-}
safeAdd cost distance
  | cost == maxBound || cost < 0 || cost > maxBound - distance = maxBound
  | otherwise = cost + distance

absInt# :: Int# -> Int#
{-# INLINE absInt# #-}
absInt# value# | isTrue# (value# <# 0#) = 0# -# value#
               | otherwise = value#

minInt#, maxInt# :: Int# -> Int# -> Int#
{-# INLINE minInt# #-}
{-# INLINE maxInt# #-}
minInt# a# b# | isTrue# (a# <# b#) = a#
              | otherwise = b#
maxInt# a# b# | isTrue# (a# <# b#) = b#
              | otherwise = a#

maxSafeCost :: Int
maxSafeCost = 2147483647 - 32767
