module ShortestPath.Exact.TileAStar.Configuration
  ( tileAStarConfigFromEnvironment
  , tileUseCTransformFromEnvironment
  ) where

import System.Environment (lookupEnv)

import ShortestPath.Exact.TileAStar.Heuristic

tileAStarConfigFromEnvironment :: IO TileAStarConfig
tileAStarConfigFromEnvironment = do
  implementation <- lookupEnv "SPM_TILE_REVERSE_IMPL" >>= parseImplementation
  compareReverse <- enabled "SPM_TILE_COMPARE_REVERSE"
  counters <- enabled "SPM_TILE_REVERSE_COUNTERS"
  pure TileAStarConfig
    { tileReverseImplementation = implementation
    , tileCompareReverseImplementations = compareReverse
    , tileCollectReverseCounters = counters
    }

tileUseCTransformFromEnvironment :: IO Bool
tileUseCTransformFromEnvironment = lookupEnv "SPM_HEURISTIC_TRANSFORM" >>= \case
  Nothing -> pure False
  Just "haskell" -> pure False
  Just "c" -> pure True
  Just value -> fail ("SPM_HEURISTIC_TRANSFORM must be haskell or c, got " <> show value)

parseImplementation :: Maybe String -> IO ReverseImplementation
parseImplementation Nothing = pure CliqueReverse
parseImplementation (Just "clique") = pure CliqueReverse
parseImplementation (Just "manhattan") = pure SparseWalkingReverse
parseImplementation (Just value) = fail ("SPM_TILE_REVERSE_IMPL must be clique or manhattan, got " <> show value)

enabled :: String -> IO Bool
enabled name = lookupEnv name >>= \case
  Nothing -> pure False
  Just "0" -> pure False
  Just "1" -> pure True
  Just value -> fail (name <> " must be 0 or 1, got " <> show value)
