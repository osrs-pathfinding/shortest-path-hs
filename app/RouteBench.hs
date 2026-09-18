{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, setNumCapabilities)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (forM, forM_, replicateM_, when)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, eitherDecode, eitherDecodeFileStrict', encode, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcess)
import System.Info (arch, os)
import System.IO (hFlush, stdout)

import ShortestPath.Account (AccountState, RequirementMode(..))
import ShortestPath.BenchmarkProfiles
import ShortestPath.Exact.ReferenceDijkstra (ReferenceDijkstra(..), findRouteReferenceDijkstra)
import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.Configuration
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport (Transport, defaultSourcePaths)
import ShortestPath.World

data RouteCase = RouteCase
  { routeId :: Maybe String
  , routeName :: String
  , routeCategory :: String
  , routeDistanceTag :: String
  , routePlaneTag :: String
  , routeStart :: [Int]
  , routeTarget :: [Int]
  , routeAllowTransports :: Bool
  , routeTiers :: [String]
  , routeNegativeProfiles :: [String]
  }

instance FromJSON RouteCase where
  parseJSON = withObject "benchmark route" $ \v ->
    RouteCase <$> v .:? "id" <*> v .: "name" <*> v .: "category" <*> v .:? "distanceTag" .!= "unknown" <*> v .:? "planeTag" .!= "unknown" <*> v .: "start" <*> v .: "target" <*> v .: "allowTransports" <*> v .:? "tiers" .!= [] <*> v .:? "negativeProfiles" .!= []

data Oracle = Oracle { oracleReachable :: Bool, oracleCost :: Maybe Int }

data PreviousResult = PreviousResult String String (Maybe Bool)

instance FromJSON PreviousResult where
  parseJSON = withObject "benchmark result" $ \v ->
    PreviousResult <$> v .: "routeId" <*> v .: "accountProfile" <*> v .:? "correct"

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
  , oracleJobs :: Int
  , rerunFailures :: Maybe FilePath
  , strictProfileVars :: Bool
  , heuristicWeightOption :: Double
  , corpusDirOption :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options "" "" "out/route-benchmark.jsonl" 3 False False False Nothing "full" 4 Nothing False 1 Nothing

main :: IO ()
main = do
  parsed <- parseOptions =<< getArgs
  corpusDir <- discoverCorpusDir (corpusDirOption parsed)
  profiles <- loadBenchmarkProfiles corpusDir
  let options = parsed
        { inputPath = if null (inputPath parsed) then corpusDir </> "corpus/routes-v1.json" else inputPath parsed
        , oraclePath = if null (oraclePath parsed) then corpusDir </> "oracle/oracle-v1.json" else oraclePath parsed
        }
  cases <- maybe id take (routeLimit options) . filterTier (benchmarkTier options) <$> loadCases options
  when (null cases) (die "no benchmark routes; select routes from the corpus first")
  world <- loadWorld defaultSourcePaths
  topology <- buildWorldTopology world
  tileConfig <- tileAStarConfigFromEnvironment
  when (strictProfileVars options) $ do
    let gaps = benchmarkProfileVariableGapsFrom profiles (allTransports world)
    when (not (null gaps)) $ die (unlines ("unmodelled benchmark profile variables:" : [name <> ": " <> show (length requirements) | (name, requirements) <- gaps]))
  if writeOracle options
    then writeOracles profiles options topology cases
    else buildTileAStarFromTopology topology >>= forceTileAStar >>= \astar -> runBench profiles tileConfig options astar cases

parseOptions :: [String] -> IO Options
parseOptions = go defaultOptions
 where
  go options [] = pure options
  go options ("--seed":rest) = go (options {inputPath = "benchmarks/routes.json", oraclePath = "out/oracle-seed.json", seedMode = True}) rest
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
  go options ("--jobs":count:rest) = case reads count of
    [(n, "")] | n > 0 -> go (options {oracleJobs = n}) rest
    _ -> die "--jobs must be a positive integer"
  go options ("--diagnostic":rest) = go (options {diagnostic = True}) rest
  go options ("--rerun-failures":path:rest) = go (options {rerunFailures = Just path}) rest
  go options ("--strict-profile-vars":rest) = go (options {strictProfileVars = True}) rest
  go options ("--corpus-dir":path:rest) = go (options {corpusDirOption = Just path}) rest
  go options ("--heuristic-weight":weight:rest) = case reads weight of
    [(n, "")] | n > 0 -> go (options {heuristicWeightOption = n}) rest
    _ -> die "--heuristic-weight must be positive"
  go _ _ = die "usage: route-bench [--corpus-dir DIR] [--seed] [--corpus PATH] [--oracle PATH] [--output PATH] [--runs N] [--tier smoke|standard|full] [--limit N] [--write-oracle] [--jobs N] [--diagnostic] [--rerun-failures JSONL] [--strict-profile-vars] [--heuristic-weight N]"

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

writeOracles :: BenchmarkProfiles -> Options -> WorldTopology -> [RouteCase] -> IO ()
writeOracles benchmarkProfiles options topology cases = do
  let profiles = [(name, benchmarkAccountFrom benchmarkProfiles name) | name <- benchmarkProfileNamesFrom benchmarkProfiles]
      work = [(route, name, profile) | route <- indexed cases, (name, profile) <- profiles]
      total = length work
      jobs = min total (oracleJobs options)
  setNumCapabilities jobs
  workQueue <- newChan
  resultQueue <- newChan
  mapM_ (writeChan workQueue . Just) work
  replicateM_ jobs (writeChan workQueue Nothing)
  replicateM_ jobs (forkIO (worker workQueue resultQueue) >> pure ())
  putProgress ("oracle: 0/" <> show total <> " jobs=" <> show jobs)
  results <- forM [1 .. total] $ \completed -> do
    outcome <- readChan resultQueue
    case outcome of
      Left exception -> throwIO exception
      Right (route, name, oracle, elapsed) -> do
        putProgress ("oracle: " <> show completed <> "/" <> show total <> " " <> stableId route <> " " <> name <> " dijkstraMs=" <> show elapsed <> " reachable=" <> show (oracleReachable oracle))
        pure (key route name, oracle)
  let entries = Map.fromList results
      reachableCount = length (filter (oracleReachable . snd) results)
  createDirectoryIfMissing True (takeDirectory (oraclePath options))
  LBS.writeFile (oraclePath options) (encode entries)
  commit <- gitCommit
  LBS.writeFile (oraclePath options <> ".metadata.json") (encode (object
    [ "formatVersion" .= (1 :: Int)
    , "accountProfilesVersion" .= (1 :: Int)
    , "routesVersion" .= (1 :: Int)
    , "oracleVersion" .= (1 :: Int)
    , "generator" .= object
        [ "implementation" .= ("shortest-path-model" :: String)
        , "gitRevision" .= commit
        , "resourceDataRevision" .= ("unspecified" :: String)
        ]
    ]))
  putStrLn ("oracle summary: " <> show reachableCount <> " reachable, " <> show (total - reachableCount) <> " unreachable; wrote " <> oraclePath options)
 where
  worker workQueue resultQueue = do
    job <- readChan workQueue
    case job of
      Nothing -> pure ()
      Just (route, name, profile) -> do
        outcome <- try (oracleFor route profile) :: IO (Either SomeException (Oracle, Double))
        writeChan resultQueue (fmap (\(oracle, elapsed) -> (route, name, oracle, elapsed)) outcome)
        worker workQueue resultQueue

  oracleFor route profile = do
    started <- getMonotonicTimeNSec
    let result = findRouteReferenceDijkstra (ReferenceDijkstra topology) (query (benchmarkNowMinutesFrom benchmarkProfiles) route profile 1)
        cost = routeCost result
    resolvedCost <- evaluate cost
    finished <- getMonotonicTimeNSec
    let reachable = resolvedCost /= maxBound
        oracle = Oracle reachable (if reachable then Just resolvedCost else Nothing)
    pure (oracle, milliseconds started finished)

runBench :: BenchmarkProfiles -> TileAStarConfig -> Options -> TileAStar -> [RouteCase] -> IO ()
runBench benchmarkProfiles tileConfig options astar cases = do
  oracles <- loadOracle options
  failedKeys <- maybe (pure Nothing) (fmap Just . loadFailedKeys) (rerunFailures options)
  commit <- gitCommit
  branch <- gitBranch
  dirty <- gitDirty
  now <- getCurrentTime
  createDirectoryIfMissing True (takeDirectory (outputPath options))
  LBS.writeFile (outputPath options) LBS.empty
  let profiles = [(name, benchmarkAccountFrom benchmarkProfiles name) | name <- benchmarkProfileNamesFrom benchmarkProfiles]
      queries =
        [ (route, profileName, profile)
        | route <- indexed cases
        , (profileName, profile) <- profiles
        , maybe True (Set.member (key route profileName)) failedKeys
        ]
      totalQueries = length queries
      negativeQueries = length [() | (route, profileName, _) <- queries, profileName `elem` routeNegativeProfiles route]
  when (null queries) (die "no failed benchmark cases selected")
  putStrLn ("benchmark population: " <> show (totalQueries - negativeQueries) <> " positive cases, " <> show negativeQueries <> " negative cases")
  -- Warm the same code path without recording it.
  let firstRoute = case queries of (route, _, _) : _ -> route; [] -> error "checked above"
  forM_ (Set.toList (Set.fromList [profileName | (_, profileName, _) <- queries])) $ \profileName -> do
    let profile = benchmarkAccountFrom benchmarkProfiles profileName
    (route, _) <- findRouteProfiledTileAStarWithConfig tileConfig astar (query (benchmarkNowMinutesFrom benchmarkProfiles) firstRoute profile (heuristicWeightOption options))
    voidRoute route
  forM_ (zip [1 :: Int ..] queries) $ \(queryNumber, (route, profileName, profile)) -> do
    putProgress ("benchmark: " <> show queryNumber <> "/" <> show totalQueries <> " " <> stableId route <> " " <> profileName)
    expected <- maybe (die ("missing oracle for " <> key route profileName <> "; run route-bench --write-oracle")) pure (Map.lookup (key route profileName) oracles)
    let expectation :: String
        expectation = if profileName `elem` routeNegativeProfiles route then "negative" else "positive"
    when (oracleReachable expected /= (expectation == "positive")) $
      die ("corpus expectation disagrees with oracle for " <> key route profileName)
    forM_ [1 .. repetitions options] $ \repetition -> do
      (result, timings) <- findRouteProfiledTileAStarWithConfig tileConfig astar (query (benchmarkNowMinutesFrom benchmarkProfiles) route profile (heuristicWeightOption options))
      let cost = routeCost result
          reachable = cost /= maxBound
          weight = heuristicWeightOption options
          correct = reachable == oracleReachable expected && (weight > 1 || not reachable || Just cost == oracleCost expected)
          quality = case (reachable, oracleCost expected) of
            (True, Just optimalCost) ->
              [ "optimalCost" .= optimalCost
              , "costGapTicks" .= (cost - optimalCost)
              , "costRatio" .= (if optimalCost == 0 then 1 else fromIntegral cost / fromIntegral optimalCost :: Double)
              , "optimal" .= (cost == optimalCost)
              ]
            _ -> []
      putProgress ("benchmark: " <> show queryNumber <> "/" <> show totalQueries <> " " <> stableId route <> " " <> profileName <> " repetition=" <> show repetition <> " tileAStarMs=" <> show (tileTotalMilliseconds timings) <> " reachable=" <> show reachable)
      when (not correct) $
        putStrLn ("oracle mismatch for " <> key route profileName)
      append (outputPath options) options $ object $
        [ "benchmarkVersion" .= ("v1" :: String), "generatedAt" .= show now, "gitCommit" .= commit, "gitBranch" .= branch, "gitDirty" .= dirty, "testbed" .= (os <> "-" <> arch)
        , "benchmarkTier" .= benchmarkTier options, "heuristicWeight" .= weight
        , "routeId" .= stableId route, "routeName" .= routeName route, "category" .= routeCategory route
        , "expectation" .= expectation
        , "distanceTag" .= routeDistanceTag route, "planeTag" .= routePlaneTag route, "allowTransports" .= routeAllowTransports route
        , "accountProfile" .= profileName, "repetition" .= repetition, "start" .= routeStart route, "target" .= routeTarget route
        , "reachable" .= reachable, "cost" .= if reachable then Just cost else Nothing
        , "expectedCost" .= oracleCost expected, "oracleReachable" .= oracleReachable expected, "oracleCost" .= oracleCost expected, "correct" .= correct
        , "timings" .= timingsJson timings, "expandedNodes" .= routeExpandedNodes result
        ] <> quality
      when (diagnostic options) $ do
        started <- getMonotonicTimeNSec
        let raw = findRouteReferenceDijkstra (ReferenceDijkstra (tileTopology astar)) (query (benchmarkNowMinutesFrom benchmarkProfiles) route profile 1)
        voidRoute raw
        finished <- getMonotonicTimeNSec
        when (routeCost raw /= routeCost result) (die ("reference Dijkstra mismatch for " <> key route profileName))
        append (outputPath options) options $ object ["routeId" .= stableId route, "accountProfile" .= profileName, "repetition" .= repetition, "diagnosticRawMs" .= milliseconds started finished]
  putStrLn ("wrote " <> outputPath options)

loadOracle :: Options -> IO (Map.Map String Oracle)
loadOracle options = do
  decoded <- eitherDecodeFileStrict' (oraclePath options)
  case decoded of
    Left _ | seedMode options -> die "seed oracle missing; run route-bench --seed --write-oracle once"
    Left message -> die (oraclePath options <> ": " <> message)
    Right value -> pure value

loadFailedKeys :: FilePath -> IO (Set.Set String)
loadFailedKeys path = do
  contents <- LBS.readFile path
  rows <- traverse decodeLine (filter (not . LBS.null) (LBS.split 10 contents))
  pure (Set.fromList [rid <> "/" <> profile | PreviousResult rid profile (Just False) <- rows])
 where
  decodeLine line =
    case eitherDecode line of
      Left message -> die (path <> ": " <> message)
      Right result -> pure result

indexed :: [RouteCase] -> [RouteCase]
indexed = zipWith add [1 :: Int ..]
 where add n route = route {routeId = Just (fromMaybe ("seed-" <> pad n) (routeId route))}
       pad n = let s = show n in replicate (4 - length s) '0' <> s

stableId :: RouteCase -> String
stableId route = fromMaybe (error "indexed route missing id") (routeId route)

key :: RouteCase -> String -> String
key route profile = stableId route <> "/" <> profile

query :: Int -> RouteCase -> Maybe AccountState -> Double -> Query
query now route profile weight =
  (defaultQuery (tile (routeStart route)) (tile (routeTarget route)))
    { allowTransports = routeAllowTransports route
    , requirementMode = maybe IgnoreRequirements ConfiguredRequirements profile
    , queryNowMinutes = now
    , heuristicWeight = weight
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

gitBranch :: IO String
gitBranch = do
  result <- readProcess "git" ["branch", "--show-current"] ""
  pure (takeWhile (/= '\n') result)

gitDirty :: IO Bool
gitDirty = do
  result <- readProcess "git" ["status", "--porcelain"] ""
  pure (not (null result))

milliseconds :: Integral a => a -> a -> Double
milliseconds start finish = fromIntegral (finish - start) / 1000000

putProgress :: String -> IO ()
putProgress message = putStrLn message >> hFlush stdout

timingsJson :: TileAStarTimings -> Value
timingsJson timings = object
  [ "mode" .= timingMode timings
  , "accountPrepareMs" .= tileAccountPrepareMilliseconds timings, "targetPrepareMs" .= tileTargetPrepareMilliseconds timings
  , "forwardSearchMs" .= tileSearchMilliseconds timings, "setupMs" .= tileHeuristicSetupMilliseconds timings
  , "reverseDijkstraMs" .= tileReverseDijkstraMilliseconds timings
  , "seedTableMs" .= tileSeedTableMilliseconds timings, "searchMs" .= tileSearchMilliseconds timings
  , "heuristicSeedCount" .= tileHeuristicSeedCount timings, "heuristicComponentCount" .= tileHeuristicComponentCount timings
  , "heuristicMaxSeedsPerComponent" .= tileHeuristicMaxSeedsPerComponent timings
  , "heuristicSeedsPerComponentP50" .= tileHeuristicSeedsPerComponentP50 timings
  , "heuristicSeedsPerComponentP90" .= tileHeuristicSeedsPerComponentP90 timings
  , "heuristicSeedsPerComponentP95" .= tileHeuristicSeedsPerComponentP95 timings
  , "heuristicSeedsPerComponentP99" .= tileHeuristicSeedsPerComponentP99 timings
  , "heuristicGeneratorCount" .= tileHeuristicGeneratorCount timings
  , "heuristicMaxGeneratorsPerComponent" .= tileHeuristicMaxGeneratorsPerComponent timings
  , "heuristicGeneratorsPerComponentP50" .= tileHeuristicGeneratorsPerComponentP50 timings
  , "heuristicGeneratorsPerComponentP90" .= tileHeuristicGeneratorsPerComponentP90 timings
  , "heuristicGeneratorsPerComponentP95" .= tileHeuristicGeneratorsPerComponentP95 timings
  , "heuristicGeneratorsPerComponentP99" .= tileHeuristicGeneratorsPerComponentP99 timings
  , "heuristicGeneratorSeedRatioP50" .= tileHeuristicGeneratorSeedRatioP50 timings
  , "heuristicGeneratorSeedRatioP90" .= tileHeuristicGeneratorSeedRatioP90 timings
  , "heuristicGeneratorSeedRatioP95" .= tileHeuristicGeneratorSeedRatioP95 timings
  , "heuristicGeneratorSeedRatioP99" .= tileHeuristicGeneratorSeedRatioP99 timings
  , "heuristicGeneratorSeedRatioMax" .= tileHeuristicGeneratorSeedRatioMax timings
  , "heuristicGeneratorSeedRatio" .= tileHeuristicGeneratorSeedRatio timings
  , "forwardAllocatedBytes" .= tileForwardAllocatedBytes timings, "forwardNsPerState" .= perState (tileSearchMilliseconds timings * 1000000)
  , "forwardAllocatedBytesPerState" .= perState (fromIntegral (tileForwardAllocatedBytes timings)), "totalMs" .= tileTotalMilliseconds timings
  , "statesPopped" .= tileStatesPopped (tileSearchCounters timings), "uniqueStatesReached" .= tileUniqueStatesReached (tileSearchCounters timings)
  , "pqPushes" .= tilePqPushes (tileSearchCounters timings), "staleEntries" .= tileStalePqEntries (tileSearchCounters timings)
  , "walkingRelaxations" .= tileWalkingRelaxations (tileSearchCounters timings), "transportRelaxations" .= tileTransportRelaxations (tileSearchCounters timings)
  , "heuristicEvaluations" .= tileHeuristicEvaluations (tileSearchCounters timings), "unreachablePrunes" .= tileHeuristicUnreachable (tileSearchCounters timings)
  , "heuristicCalls" .= tileHeuristicCalls (tileSearchCounters timings)
  , "heuristicCandidatesScanned" .= tileHeuristicCandidatesScanned (tileSearchCounters timings)
  , "heuristicMaxCandidatesPerCall" .= tileHeuristicMaxCandidatesPerCall (tileSearchCounters timings)
  , "reverseStatesSettled" .= reverseStatesPopped (tileReverseCounters timings)
  , "reverseEdgesRelaxed" .= reverseEdgesRelaxed (tileReverseCounters timings)
  , "reversePqPushes" .= reversePqPushes (tileReverseCounters timings)
  , "reversePqStalePops" .= reverseStalePqEntries (tileReverseCounters timings)
  , "reversePqMaxSize" .= reversePqMaxSize (tileReverseCounters timings)
  , "unknownComponentPrunes" .= tileUnknownComponentPrunes (tileSearchCounters timings), "noReverseSeedPrunes" .= tileNoReverseSeedPrunes (tileSearchCounters timings)
  , "bestBankUpdates" .= tileBestBankCostUpdates (tileSearchCounters timings), "finalBestBankCost" .= tileFinalBestBankCost (tileSearchCounters timings)
  ]
 where
  perState :: Double -> Maybe Double
  perState total
    | states == 0 = Nothing
    | otherwise = Just (total / fromIntegral states)
  states = tileStatesPopped (tileSearchCounters timings)

timingMode :: TileAStarTimings -> String
timingMode timings
  | tileTargetPrepareMilliseconds timings == 0 = "warm-target"
  | tileAccountPrepareMilliseconds timings == 0 = "warm-account"
  | otherwise = "cold"

die :: String -> IO a
die message = putStrLn message >> exitFailure
