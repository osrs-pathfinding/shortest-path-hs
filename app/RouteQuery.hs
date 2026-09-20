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
    putStrLn ("forward counters: " <> show (tileSearchCounters timings))
    putStrLn ("reverse counters: " <> show (tileReverseCounters timings))

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
