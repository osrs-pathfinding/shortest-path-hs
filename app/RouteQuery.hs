module Main (main) where

import Control.Monad (when)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

import ShortestPath.Account (RequirementMode(..))
import ShortestPath.BenchmarkProfiles
import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.Configuration (tileAStarConfigFromEnvironment)
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport (defaultSourcePaths)
import ShortestPath.World (loadWorld)

data Options = Options
  { profileName :: String
  , start :: Tile
  , target :: Tile
  , printCounters :: Bool
  , corpusDir :: Maybe FilePath
  }

main :: IO ()
main = do
  options <- parseOptions =<< getArgs
  corpus <- discoverCorpusDir (corpusDir options)
  profiles <- loadBenchmarkProfiles corpus
  account <- maybe
    (die ("unknown profile " <> show (profileName options) <> "; expected " <> intercalate ", " (benchmarkProfileNamesFrom profiles)))
    pure
    (benchmarkAccountFrom profiles (profileName options))
  world <- loadWorld defaultSourcePaths
  astar <- buildTileAStar world
  environmentConfig <- tileAStarConfigFromEnvironment
  let config = environmentConfig {tileCollectReverseCounters = printCounters options}
      query = (defaultQuery (start options) (target options))
        { requirementMode = ConfiguredRequirements account
        , queryNowMinutes = benchmarkNowMinutesFrom profiles
        }
  (route, timings) <- findRouteProfiledTileAStarWithConfig config astar query
  putStrLn ("profile: " <> profileName options)
  putStrLn ("start: " <> coordinateText (start options))
  putStrLn ("target: " <> coordinateText (target options))
  if routeCost route == maxBound
    then putStrLn "unreachable"
    else do
      putStrLn ("cost: " <> show (routeCost route))
      putStrLn ("expanded nodes: " <> show (routeExpandedNodes route))
      putStrLn "route:"
      putStrLn ("  start " <> coordinateText (start options))
      mapM_ printStep (routeSteps route)
  when (printCounters options) $ do
    printMetrics route timings

printMetrics :: Route -> TileAStarTimings -> IO ()
printMetrics route timings = do
  let forward = tileSearchCounters timings
      reverseCounters = tileReverseCounters timings
      metric name value = putStrLn ("  " <> name <> ": " <> show value)
  putStrLn "metrics:"
  metric "total_ms" (tileTotalMilliseconds timings)
  metric "account_prepare_ms" (tileAccountPrepareMilliseconds timings)
  metric "target_prepare_inclusive_ms" (tileTargetPrepareMilliseconds timings)
  metric "reverse_ms" (tileReverseDijkstraMilliseconds timings)
  metric "target_prepare_non_reverse_ms"
    (tileTargetPrepareMilliseconds timings - tileReverseDijkstraMilliseconds timings)
  metric "search_ms" (tileSearchMilliseconds timings)
  metric "route_cost" (routeCost route)
  metric "route_steps" (length (routeSteps route))
  metric "nodes_expanded" (routeExpandedNodes route)
  metric "states_popped" (tileStatesPopped forward)
  metric "unique_states_reached" (tileUniqueStatesReached forward)
  metric "pq_pushes" (tilePqPushes forward)
  metric "walking_relaxations" (tileWalkingRelaxations forward)
  metric "transport_relaxations" (tileTransportRelaxations forward)
  metric "heuristic_evaluations" (tileHeuristicEvaluations forward)
  metric "reverse_states_popped" (reverseStatesPopped reverseCounters)
  metric "reverse_edges_relaxed" (reverseEdgesRelaxed reverseCounters)
  metric "reverse_pq_pushes" (reversePqPushes reverseCounters)
  metric "reverse_stale_pq_entries" (reverseStalePqEntries reverseCounters)

printStep :: RouteStep -> IO ()
printStep (Walk tile) = putStrLn ("  walk -> " <> coordinateText tile)
printStep (UseTransport label tile) = putStrLn ("  transport " <> show label <> " -> " <> coordinateText tile)

parseOptions :: [String] -> IO Options
parseOptions arguments = do
  let (counters, corpus, positional) = collect False Nothing [] arguments
  case positional of
    [profile, sx, sy, sp, tx, ty, tp] ->
      Options profile <$> tile "start" sx sy sp <*> tile "target" tx ty tp <*> pure counters <*> pure corpus
    _ -> usage
 where
  collect counters corpus positional [] = (counters, corpus, reverse positional)
  collect _ _ _ ["--corpus-dir"] = (False, Nothing, [])
  collect _ corpus positional ("--counters":rest) = collect True corpus positional rest
  collect counters _ positional ("--corpus-dir":path:rest) = collect counters (Just path) positional rest
  collect counters corpus positional (argument:rest) = collect counters corpus (argument:positional) rest

  tile label xText yText planeText = do
    x <- integer (label <> " x") xText
    y <- integer (label <> " y") yText
    plane <- integer (label <> " plane") planeText
    when (x < 0 || x > 32767 || y < 0 || y > 32767 || plane < 0 || plane > 3) $
      die (label <> " must be within x/y 0..32767 and plane 0..3")
    pure (packTile x y plane)

  integer label value = maybe (die (label <> " must be an integer")) pure (readMaybe value)

  usage = die "usage: route-query PROFILE START_X START_Y START_PLANE TARGET_X TARGET_Y TARGET_PLANE [--counters] [--corpus-dir DIR]"
