module Main (main) where

import Control.Monad (when)
import Data.List (find, intercalate)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

import ShortestPath.Account (RequirementMode(..))
import ShortestPath.BenchmarkCorpus
import ShortestPath.BenchmarkProfiles
import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.Cache (loadOrBuildTileAStar)
import ShortestPath.Exact.TileAStar.Configuration (tileAStarConfigFromEnvironment)
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport (defaultSourcePaths)
import ShortestPath.World (loadWorld)

data RouteSource
  = ExplicitCoordinates Tile Tile
  | CorpusRoute String

data Options = Options
  { profileName :: String
  , routeSource :: RouteSource
  , printCounters :: Bool
  , corpusDir :: Maybe FilePath
  }

data Parsed = Parsed
  { parsedProfile :: Maybe String
  , parsedRoute :: Maybe String
  , parsedStart :: Maybe String
  , parsedTarget :: Maybe String
  , parsedCounters :: Bool
  , parsedCorpus :: Maybe FilePath
  , parsedPositional :: [String]
  }

main :: IO ()
main = do
  options <- parseOptions =<< getArgs
  corpus <- discoverCorpusDir (corpusDir options)
  profiles <- loadBenchmarkProfiles corpus
  (selectedRoute, startTile, targetTile) <- resolveRoute (routeSource options) corpus
  account <- maybe
    (die ("unknown profile " <> show (profileName options) <> "; expected " <> intercalate ", " (benchmarkProfileNamesFrom profiles)))
    pure
    (benchmarkAccountFrom profiles (profileName options))
  world <- loadWorld defaultSourcePaths
  astar <- loadOrBuildTileAStar world
  environmentConfig <- tileAStarConfigFromEnvironment
  let config = environmentConfig {tileCollectReverseCounters = printCounters options}
      query = (defaultQuery startTile targetTile)
        { requirementMode = ConfiguredRequirements account
        , queryNowMinutes = benchmarkNowMinutesFrom profiles
        }
  (route, timings) <- findRouteProfiledTileAStarWithConfig config astar query
  putStrLn ("profile: " <> profileName options)
  maybe (pure ()) (putStrLn . ("corpus route: " <>)) selectedRoute
  putStrLn ("start: " <> coordinateText startTile)
  putStrLn ("target: " <> coordinateText targetTile)
  if routeCost route == maxBound
    then putStrLn "unreachable"
    else do
      putStrLn ("cost: " <> show (routeCost route))
      putStrLn ("expanded nodes: " <> show (routeExpandedNodes route))
      putStrLn "route:"
      putStrLn ("  start " <> coordinateText startTile)
      mapM_ printStep (routeSteps route)
  when (printCounters options) $ do
    printMetrics route timings

resolveRoute :: RouteSource -> FilePath -> IO (Maybe String, Tile, Tile)
resolveRoute (ExplicitCoordinates startTile targetTile) _ = pure (Nothing, startTile, targetTile)
resolveRoute (CorpusRoute selector) corpus = do
  routes <- loadBenchmarkRoutes corpus
  route <- maybe (die ("Unknown corpus route: " <> selector)) pure
    (find (matches selector) routes)
  startTile <- corpusTile "start" (routeStart route)
  targetTile <- corpusTile "target" (routeTarget route)
  pure (Just selector, startTile, targetTile)
 where
  matches wanted route = routeId route == Just wanted || routeName route == wanted

corpusTile :: String -> [Int] -> IO Tile
corpusTile label values = case values of
  [x, y, plane] -> checkedTile label x y plane
  _ -> die (label <> " in corpus route must contain x, y, and plane")

checkedTile :: String -> Int -> Int -> Int -> IO Tile
checkedTile label x y plane = do
  when (x < 0 || x > 32767 || y < 0 || y > 32767 || plane < 0 || plane > 3) $
    die (label <> " must be within x/y 0..32767 and plane 0..3")
  pure (packTile x y plane)

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
  parsed <- collect emptyParsed (dropCommand arguments)
  let positional = reverse (parsedPositional parsed)
  case parsed {parsedPositional = positional} of
    Parsed (Just profile) (Just selector) Nothing Nothing counters corpus [] ->
      pure (Options profile (CorpusRoute selector) counters corpus)
    Parsed (Just profile) Nothing (Just startText) (Just targetText) counters corpus [] -> do
      startTile <- point "start" startText
      targetTile <- point "target" targetText
      pure (Options profile (ExplicitCoordinates startTile targetTile) counters corpus)
    Parsed Nothing Nothing Nothing Nothing counters corpus [profile, sx, sy, sp, tx, ty, tp] -> do
      startTile <- tile "start" sx sy sp
      targetTile <- tile "target" tx ty tp
      pure (Options profile (ExplicitCoordinates startTile targetTile) counters corpus)
    _ -> usage
 where
  dropCommand ("route":rest) = rest
  dropCommand ("route-query":rest) = rest
  dropCommand rest = rest

  emptyParsed = Parsed Nothing Nothing Nothing Nothing False Nothing []

  collect parsed [] = pure parsed
  collect _ ("--corpus-dir":[]) = usage
  collect parsed ("--corpus-dir":path:rest) = collect parsed {parsedCorpus = Just path} rest
  collect parsed ("--counters":rest) = collect parsed {parsedCounters = True} rest
  collect parsed ("--profile":profile:rest) = collect parsed {parsedProfile = Just profile} rest
  collect parsed ("--route":route:rest) = collect parsed {parsedRoute = Just route} rest
  collect parsed ("--start":startText:rest) = collect parsed {parsedStart = Just startText} rest
  collect parsed ("--end":targetText:rest) = collect parsed {parsedTarget = Just targetText} rest
  collect parsed (argument:rest) = collect parsed {parsedPositional = argument : parsedPositional parsed} rest

  point label value = case commaParts value of
    [x, y, plane] -> do
      x' <- integer (label <> " x") x
      y' <- integer (label <> " y") y
      plane' <- integer (label <> " plane") plane
      checkedTile label x' y' plane'
    _ -> die (label <> " must be X,Y,PLANE")

  commaParts value = case break (== ',') value of
    (part, []) -> [part]
    (part, _:rest) -> part : commaParts rest

  tile label xText yText planeText = do
    x <- integer (label <> " x") xText
    y <- integer (label <> " y") yText
    plane <- integer (label <> " plane") planeText
    checkedTile label x y plane

  integer label value = maybe (die (label <> " must be an integer")) pure (readMaybe value)

  usage = die "usage: route-query --corpus-dir DIR --route ROUTE --profile PROFILE | route-query --corpus-dir DIR --start X,Y,PLANE --end X,Y,PLANE --profile PROFILE"
