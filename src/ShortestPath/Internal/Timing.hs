module ShortestPath.Internal.Timing
  ( milliseconds
  , timedIO
  ) where

import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

timedIO :: (a -> IO b) -> IO a -> IO (a, Double)
timedIO force action = do
  started <- getMonotonicTimeNSec
  value <- action
  _ <- force value
  finished <- getMonotonicTimeNSec
  pure (value, milliseconds started finished)

milliseconds :: Word64 -> Word64 -> Double
milliseconds started finished = fromIntegral (finished - started) / 1000000
