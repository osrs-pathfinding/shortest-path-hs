module ShortestPath.Exact.TileAStar.Configuration
  ( tileAStarConfigFromEnvironment
  , tileUseCTransformFromEnvironment
  ) where

import System.Environment (lookupEnv)

import ShortestPath.Exact.TileAStar.Heuristic

tileAStarConfigFromEnvironment :: IO TileAStarConfig
tileAStarConfigFromEnvironment = do
  implementation <- lookupEnv "SPM_TILE_REVERSE_IMPL" >>= parseImplementation
  gatewayMode <- enabled False "SPM_MANHATTAN_GATEWAYS"
  compareReverse <- enabled False "SPM_TILE_COMPARE_REVERSE"
  counters <- enabled False "SPM_TILE_REVERSE_COUNTERS"
  pure TileAStarConfig
    { tileReverseImplementation = implementation
    , tileManhattanHeuristicMode = if gatewayMode then ManhattanGateways else ManhattanSeedScan
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
parseImplementation Nothing = pure SparseWalkingReverse
parseImplementation (Just "clique") = pure CliqueReverse
parseImplementation (Just "manhattan") = pure SparseWalkingReverse
parseImplementation (Just value) = fail ("SPM_TILE_REVERSE_IMPL must be clique or manhattan, got " <> show value)

enabled :: Bool -> String -> IO Bool
enabled defaultValue name = lookupEnv name >>= \case
  Nothing -> pure defaultValue
  Just "0" -> pure False
  Just "1" -> pure True
  Just value -> fail (name <> " must be 0 or 1, got " <> show value)
