module Main (main) where

import Control.Exception (evaluate)
import Data.Bits ((.&.))
import qualified Data.Vector.Unboxed as Vector
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)

import ShortestPath.Exact.TileAStar.HeuristicScan
import ShortestPath.Tile (packTile, unTile)

main :: IO ()
main = do
  putStrLn "size implementation ns/scan million-candidates/s"
  mapM_ benchmark [1, 2, 3, 4, 8, 16, 32, 64, 128, 256, 612]
 where
  benchmark size = do
    let bucket = generatorScanFromVector $ Vector.generate size $ \ix ->
          ( unTile (packTile ((ix * 101) .&. 0x7fff) ((ix * 211) .&. 0x7fff) 0)
          , (ix * 17) `mod` 100000
          )
        scans = max 20000 (10000000 `div` size)
    scalar <- measure scans size bucket scanGeneratorsScalar
    simd <- measure scans size bucket scanGeneratorsSimd
    printResult size "scalar" scalar
    printResult size "simd" simd

measure :: Int -> Int -> GeneratorScan -> (GeneratorScan -> Int -> Int -> Int) -> IO (Double, Double)
measure scans size bucket scanner = do
  started <- getMonotonicTimeNSec
  checksum <- evaluate (go 0 0)
  finished <- getMonotonicTimeNSec
  _ <- evaluate checksum
  let elapsed = fromIntegral (finished - started)
  pure (elapsed / fromIntegral scans, fromIntegral (scans * size) * 1000 / elapsed)
 where
  go ix total
    | ix >= scans = total
    | otherwise = go (ix + 1) (total + scanner bucket ((ix * 17) .&. 0x7fff) ((ix * 29) .&. 0x7fff))

printResult :: Int -> String -> (Double, Double) -> IO ()
printResult size implementation (nsPerScan, millionCandidatesPerSecond) =
  printf "%4d %-6s %10.2f %20.2f\n" size implementation nsPerScan millionCandidatesPerSecond
