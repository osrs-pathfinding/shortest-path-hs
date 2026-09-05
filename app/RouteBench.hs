{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, eitherDecodeFileStrict', encode, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.Process (readProcess)
import System.Info (arch, os)
import System.IO (hFlush, stdout)

import ShortestPath.Account (AccountBuild, RequirementMode(..))
import ShortestPath.BenchmarkProfiles (benchmarkAccount, benchmarkProfileNames)
import ShortestPath.Exact.RawDijkstra (RawDijkstra(..))
import ShortestPath.Exact.TileAStar
import ShortestPath.Pathfinder hiding (routeName)
import ShortestPath.Tile
import ShortestPath.Transport (Transport, defaultSourcePaths)
import ShortestPath.World

data RouteCase = RouteCase
  { routeId :: Maybe String
  , routeName :: String
  , routeCategory :: String
  , routeStart :: [Int]
  , routeTarget :: [Int]
  , routeAllowTransports :: Bool
  , routeTiers :: [String]
  }

instance FromJSON RouteCase where
  parseJSON = withObject "benchmark route" $ \v ->
    RouteCase <$> v .:? "id" <*> v .: "name" <*> v .: "category" <*> v .: "start" <*> v .: "target" <*> v .: "allowTransports" <*> v .:? "tiers" .!= []

data Oracle = Oracle { oracleReachable :: Bool, oracleCost :: Maybe Int }

instance FromJSON Oracle where
  parseJSON = withObject "oracle" $ \v -> Oracle <$> v .: "reachable" <*> v .:? "cost"

instance ToJSON Oracle where
  toJSON o = object ["reachable" .= oracleReachable o, "cost" .= oracleCost o]

data Options = Options
  { inputPath :: FilePath
  , oraclePath :: FilePath
  , outputPath :: FilePath
  , repetitions :: Int
  , seedMode :: Bool
  , writeOracle :: Bool
  , diagnostic :: Bool
  , routeLimit :: Maybe Int
  , benchmarkTier :: String
  }

defaultOptions :: Options
defaultOptions = Options "benchmarks/corpus/routes-v1.json" "benchmarks/corpus/oracle-v1.json" "out/route-benchmark.jsonl" 3 False False False Nothing "full"

main :: IO ()
main = do
  options <- parseOptions =<< getArgs
  cases <- maybe id take (routeLimit options) . filterTier (benchmarkTier options) <$> loadCases options
  when (null cases) (die "no benchmark routes; select routes for benchmarks/corpus/routes-v1.json first")
  world <- loadWorld defaultSourcePaths
  astar <- buildTileAStar world >>= forceTileAStar
  if writeOracle options
    then writeOracles options world cases
    else runBench options world astar cases

parseOptions :: [String] -> IO Options
parseOptions = go defaultOptions
 where
  go options [] = pure options
  go options ("--seed":rest) = go (options {inputPath = "benchmarks/routes.json", oraclePath = "benchmarks/corpus/oracle-seed.json", seedMode = True}) rest
  go options ("--corpus":path:rest) = go (options {inputPath = path}) rest
  go options ("--oracle":path:rest) = go (options {oraclePath = path}) rest
  go options ("--output":path:rest) = go (options {outputPath = path}) rest
  go options ("--runs":count:rest) = case reads count of
    [(n, "")] | n > 0 -> go (options {repetitions = n}) rest
    _ -> die "--runs must be a positive integer"
  go options ("--limit":count:rest) = case reads count of
    [(n, "")] | n > 0 -> go (options {routeLimit = Just n}) rest
    _ -> die "--limit must be a positive integer"
  go options ("--tier":tier:rest)
    | tier `elem` ["smoke", "standard", "full"] = go (options {benchmarkTier = tier}) rest
    | otherwise = die "--tier must be smoke, standard, or full"
  go options ("--write-oracle":rest) = go (options {writeOracle = True}) rest
  go options ("--diagnostic":rest) = go (options {diagnostic = True}) rest
  go _ _ = die "usage: route-bench [--seed] [--corpus PATH] [--oracle PATH] [--output PATH] [--runs N] [--tier smoke|standard|full] [--limit N] [--write-oracle] [--diagnostic]"

loadCases :: Options -> IO [RouteCase]
loadCases options = do
  decoded <- eitherDecodeFileStrict' (inputPath options)
  case decoded of
    Left message -> die (inputPath options <> ": " <> message)
    Right cases
      | not (seedMode options) && any (maybe True null . routeId) cases -> die "selected corpus routes require stable ids"
      | otherwise -> pure cases

filterTier :: String -> [RouteCase] -> [RouteCase]
filterTier "full" = id
filterTier tier = filter (elem tier . routeTiers)

writeOracles :: Options -> World -> [RouteCase] -> IO ()
writeOracles options world cases = do
  let profiles = [(name, benchmarkAccount name (allTransports world)) | name <- benchmarkProfileNames]
      work = [(route, name, profile) | route <- indexed cases, (name, profile) <- profiles]
      total = length work
  putProgress ("oracle: 0/" <> show total)
  entries <- go total (0 :: Int) Map.empty work
  createDirectoryIfMissing True (takeDirectory (oraclePath options))
  LBS.writeFile (oraclePath options) (encode entries)
  putStrLn ("wrote " <> oraclePath options)
 where
  go _ _ entries [] = pure entries
  go total n entries ((route, name, profile):rest) = do
    let result = findRoute (RawDijkstra world) (query route profile)
        cost = routeCost result
    resolvedCost <- evaluate cost
    let reachable = resolvedCost /= maxBound
        oracle = Oracle reachable (if reachable then Just resolvedCost else Nothing)
    putProgress ("oracle: " <> show (n + 1) <> "/" <> show total <> " " <> stableId route <> " " <> name)
    go total (n + 1) (Map.insert (key route name) oracle entries) rest

runBench :: Options -> World -> TileAStar -> [RouteCase] -> IO ()
runBench options world astar cases = do
  oracles <- loadOracle options
  commit <- gitCommit
  now <- getCurrentTime
  createDirectoryIfMissing True (takeDirectory (outputPath options))
  LBS.writeFile (outputPath options) LBS.empty
  let profiles = [(name, benchmarkAccount name (allTransports world)) | name <- benchmarkProfileNames]
      totalQueries = length cases * length profiles
  -- Warm the same code path without recording it.
  let firstRoute = case indexed cases of route : _ -> route; [] -> error "checked above"
  forM_ profiles $ \(_, profile) -> do
    (route, _) <- findRouteProfiledTileAStar astar (query firstRoute profile)
    voidRoute route
  forM_ (zip [1 :: Int ..] [(route, profileName, profile) | route <- indexed cases, (profileName, profile) <- profiles]) $ \(queryNumber, (route, profileName, profile)) -> do
    putProgress ("benchmark: " <> show queryNumber <> "/" <> show totalQueries <> " " <> stableId route <> " " <> profileName)
    expected <- maybe (die ("missing oracle for " <> key route profileName <> "; run route-bench --write-oracle")) pure (Map.lookup (key route profileName) oracles)
    forM_ [1 .. repetitions options] $ \repetition -> do
      (result, timings) <- findRouteProfiledTileAStar astar (query route profile)
      let reachable = routeCost result /= maxBound
      when (reachable /= oracleReachable expected || (reachable && Just (routeCost result) /= oracleCost expected)) $
        die ("oracle mismatch for " <> key route profileName)
      append (outputPath options) options $ object
        [ "benchmarkVersion" .= ("v1" :: String), "generatedAt" .= show now, "gitCommit" .= commit, "testbed" .= (os <> "-" <> arch)
        , "routeId" .= stableId route, "routeName" .= routeName route, "category" .= routeCategory route
        , "accountProfile" .= profileName, "repetition" .= repetition, "start" .= routeStart route, "target" .= routeTarget route
        , "reachable" .= reachable, "cost" .= if reachable then Just (routeCost result) else Nothing
        , "timings" .= timingsJson timings, "expandedNodes" .= routeExpandedNodes result
        ]
      when (diagnostic options) $ do
        started <- getMonotonicTimeNSec
        let raw = findRoute (RawDijkstra world) (query route profile)
        voidRoute raw
        finished <- getMonotonicTimeNSec
        when (routeCost raw /= routeCost result) (die ("raw Dijkstra mismatch for " <> key route profileName))
        append (outputPath options) options $ object ["routeId" .= stableId route, "accountProfile" .= profileName, "repetition" .= repetition, "diagnosticRawMs" .= milliseconds started finished]
  putStrLn ("wrote " <> outputPath options)

loadOracle :: Options -> IO (Map.Map String Oracle)
loadOracle options = do
  decoded <- eitherDecodeFileStrict' (oraclePath options)
  case decoded of
    Left _ | seedMode options -> die "seed oracle missing; run route-bench --seed --write-oracle once"
    Left message -> die (oraclePath options <> ": " <> message)
    Right value -> pure value

indexed :: [RouteCase] -> [RouteCase]
indexed = zipWith add [1 :: Int ..]
 where add n route = route {routeId = Just (fromMaybe ("seed-" <> pad n) (routeId route))}
       pad n = let s = show n in replicate (4 - length s) '0' <> s

stableId :: RouteCase -> String
stableId route = fromMaybe (error "indexed route missing id") (routeId route)

key :: RouteCase -> String -> String
key route profile = stableId route <> "/" <> profile

query :: RouteCase -> Maybe AccountBuild -> Query
query route profile =
  (defaultQuery (tile (routeStart route)) (tile (routeTarget route)))
    { allowTransports = routeAllowTransports route
    , requirementMode = maybe IgnoreRequirements ConfiguredRequirements profile
    , queryNowMinutes = 100000000
    }
 where
  tile [x, y, plane] = packTile x y plane
  tile _ = error ("invalid coordinate for " <> routeName route)

allTransports :: World -> [Transport]
allTransports world = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world

append :: FilePath -> Options -> Value -> IO ()
append path _ value = LBS.appendFile path (encode value <> "\n")

voidRoute :: Route -> IO ()
voidRoute route = evaluate (routeCost route + routeExpandedNodes route + length (routeSteps route)) >> pure ()

gitCommit :: IO String
gitCommit = do
  result <- readProcess "git" ["rev-parse", "--short", "HEAD"] ""
  pure (takeWhile (/= '\n') result)

milliseconds :: Integral a => a -> a -> Double
milliseconds start finish = fromIntegral (finish - start) / 1000000

putProgress :: String -> IO ()
putProgress message = putStrLn message >> hFlush stdout

timingsJson :: TileAStarTimings -> Value
timingsJson timings = object
  [ "setupMs" .= tileHeuristicSetupMilliseconds timings, "reverseDijkstraMs" .= tileReverseDijkstraMilliseconds timings
  , "seedTableMs" .= tileSeedTableMilliseconds timings, "searchMs" .= tileSearchMilliseconds timings, "totalMs" .= tileTotalMilliseconds timings
  , "statesPopped" .= tileStatesPopped (tileSearchCounters timings), "uniqueStatesReached" .= tileUniqueStatesReached (tileSearchCounters timings)
  , "pqPushes" .= tilePqPushes (tileSearchCounters timings), "staleEntries" .= tileStalePqEntries (tileSearchCounters timings)
  , "walkingRelaxations" .= tileWalkingRelaxations (tileSearchCounters timings), "transportRelaxations" .= tileTransportRelaxations (tileSearchCounters timings)
  , "heuristicEvaluations" .= tileHeuristicEvaluations (tileSearchCounters timings), "unreachablePrunes" .= tileHeuristicUnreachable (tileSearchCounters timings)
  , "unknownComponentPrunes" .= tileUnknownComponentPrunes (tileSearchCounters timings), "noReverseSeedPrunes" .= tileNoReverseSeedPrunes (tileSearchCounters timings)
  , "bestBankUpdates" .= tileBestBankCostUpdates (tileSearchCounters timings), "finalBestBankCost" .= tileFinalBestBankCost (tileSearchCounters timings)
  ]

die :: String -> IO a
die message = putStrLn message >> exitFailure
