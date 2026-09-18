{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Aeson (Value(..), eitherDecodeStrict', encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcess)
import ShortestPath.BenchmarkProfiles (discoverCorpusDir)

data Options = Options
  { inputPath :: FilePath, corpusPath :: FilePath, exclusionsPath :: FilePath, profileSource :: FilePath
  , runId :: Maybe String, notes :: String, testbed :: String, clickhouseUrl :: String
  , sweep :: Maybe String, corpusDirOption :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options "" "" "" "" Nothing "" "cedric" "http://127.0.0.1:8123" Nothing Nothing

main :: IO ()
main = do
  parsed <- parseOptions defaultOptions =<< getArgs
  corpusRoot <- discoverCorpusDir (corpusDirOption parsed)
  let options = parsed
        { corpusPath = if null (corpusPath parsed) then corpusRoot <> "/corpus/routes-v1.json" else corpusPath parsed
        , exclusionsPath = if null (exclusionsPath parsed) then corpusRoot <> "/corpus/excluded-routes-v1.json" else exclusionsPath parsed
        , profileSource = if null (profileSource parsed) then corpusRoot <> "/accounts/account-profiles-v1.json" else profileSource parsed
        }
  when (null (inputPath options)) dieUsage
  raw <- BS.readFile (inputPath options)
  rows <- either die pure (mapM decodeLine (filter (not . BS.null) (BS.split 10 raw)))
  when (null rows) (die "input JSONL is empty")
  unless (all validResult rows) (die "every result must contain expectation=positive|negative, reachable, and oracleReachable")
  let first = case rows of (row, _):_ -> row; [] -> error "checked above"
      rid = fromMaybe (generatedRunId first) (runId options)
      samples = map (sampleRow rid) rows
  corpus <- BS.readFile (corpusPath options)
  exclusions <- BS.readFile (exclusionsPath options)
  excludedRoutes <- either die pure (eitherDecodeStrict' exclusions :: Either String [Value])
  profiles <- BS.readFile (profileSource options)
  corpusId <- gitHash (corpus <> "\n" <> exclusions)
  profileId <- gitHash profiles
  corpusRevision <- readProcess "git" ["-C", corpusRoot, "rev-parse", "HEAD"] ""
  let tier = text first "benchmarkTier" "full"
      suiteInput = BSC.pack (corpusId <> "\n" <> profileId <> "\n" <> tier)
  suiteId <- gitHash suiteInput
  host <- fromMaybe "unknown" <$> lookupEnv "HOSTNAME"
  duplicate <- clickhouse options ("SELECT count() FROM osrs_bench.runs WHERE run_id = '" <> sqlString rid <> "'") LBS.empty
  unless (trim duplicate == "0") (die ("complete run already exists: " <> rid))
  _ <- clickhouse options "INSERT INTO osrs_bench.samples FORMAT JSONEachRow" (LBS.intercalate "\n" (map encode samples) <> "\n")
  now <- getCurrentTime
  let run = object
        [ "run_id" .= rid, "created_at" .= formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" now
        , "git_commit" .= text first "gitCommit" "", "git_branch" .= text first "gitBranch" ""
        , "git_dirty" .= (if bool first "gitDirty" False then (1 :: Int) else 0), "corpus_id" .= corpusId, "profile_set_id" .= profileId
        , "suite_id" .= suiteId, "benchmark_tier" .= tier
        , "route_count" .= routeCount rows, "case_count" .= caseCount rows
        , "testbed" .= testbed options, "hostname" .= host, "runner_version" .= text first "benchmarkVersion" ""
        , "notes" .= notes options
        , "metadata" .= (Map.fromList
            ([ ("source", inputPath options)
             , ("heuristic_weight", show (numberDouble first "heuristicWeight" 1))
             , ("positive_case_count", show (caseCountFor "positive" rows))
             , ("negative_case_count", show (caseCountFor "negative" rows))
             , ("excluded_route_count", show (length excludedRoutes))
             , ("corpus_format_version", "1")
             , ("account_profiles_version", "1")
             , ("corpus_git_revision", trim corpusRevision)
             ] <> maybe [] (\value -> [("sweep", value)]) (sweep options)) :: Map.Map String String)
        ]
  _ <- clickhouse options "INSERT INTO osrs_bench.runs FORMAT JSONEachRow" (encode run <> "\n")
  putStrLn ("Imported run " <> rid <> " (" <> show (length rows) <> " samples, suite " <> suiteId <> ")")

parseOptions :: Options -> [String] -> IO Options
parseOptions o [] = pure o
parseOptions o ("--run-id":v:xs) = parseOptions (o {runId = Just v}) xs
parseOptions o ("--notes":v:xs) = parseOptions (o {notes = v}) xs
parseOptions o ("--testbed":v:xs) = parseOptions (o {testbed = v}) xs
parseOptions o ("--clickhouse-url":v:xs) = parseOptions (o {clickhouseUrl = v}) xs
parseOptions o ("--corpus":v:xs) = parseOptions (o {corpusPath = v}) xs
parseOptions o ("--exclusions":v:xs) = parseOptions (o {exclusionsPath = v}) xs
parseOptions o ("--profile-source":v:xs) = parseOptions (o {profileSource = v}) xs
parseOptions o ("--sweep":v:xs) = parseOptions (o {sweep = Just v}) xs
parseOptions o ("--corpus-dir":v:xs) = parseOptions (o {corpusDirOption = Just v}) xs
parseOptions o (v:xs) | null (inputPath o) = parseOptions (o {inputPath = v}) xs
parseOptions _ _ = dieUsage

decodeLine :: BS.ByteString -> Either String (Value, String)
decodeLine line = (, Text.unpack (Text.Encoding.decodeUtf8 line)) <$> eitherDecodeStrict' line

sampleRow :: String -> (Value, String) -> Value
sampleRow rid (raw, rawJson) = object
  [ "run_id" .= rid, "profile" .= text raw "accountProfile" ""
  , "route_id" .= text raw "routeId" "", "route_label" .= text raw "routeName" ""
  , "category" .= text raw "category" "unknown", "distance_tag" .= text raw "distanceTag" "unknown"
  , "plane_tag" .= text raw "planeTag" "unknown", "sample_index" .= (number raw "repetition" 1 - 1)
  , "expectation" .= text raw "expectation" ""
  , "reachable" .= (if bool raw "reachable" False then (1 :: Int) else 0)
  , "cost" .= numberMaybe raw "cost"
  , "oracle_reachable" .= (if bool raw "oracleReachable" False then (1 :: Int) else 0)
  , "oracle_cost" .= firstNumberMaybe raw ["oracleCost", "expectedCost"]
  , "status" .= status raw, "correct" .= (if bool raw "correct" True then (1 :: Int) else 0)
  , "metrics" .= metrics raw, "dimensions" .= dimensions raw, "raw_json" .= rawJson
  ]

metrics :: Value -> Map.Map String Double
metrics raw = Map.fromList
  ( timingMetrics raw
  <> scalar "expanded_nodes" "expandedNodes"
  <> scalar "path_cost" "cost"
  <> firstScalar "expected_path_cost" ["optimalCost", "expectedCost"]
  <> scalar "cost_gap_ticks" "costGapTicks"
  <> scalarDouble "cost_ratio" "costRatio"
  <> booleanMetric "optimal" "optimal"
  )
 where
  timingMetrics (Object o) = case KeyMap.lookup "timings" o of Just (Object t) -> [(metricName (Key.toString k), toRealFloat n) | (k, Number n) <- KeyMap.toList t]; _ -> []
  timingMetrics _ = []
  scalar name key = case numberMaybe raw key of Just n -> [(name, fromIntegral n)]; Nothing -> []
  firstScalar _ [] = []
  firstScalar name (key:keys) = case scalar name key of [] -> firstScalar name keys; value -> value
  scalarDouble name key = case numberDoubleMaybe raw key of Just n -> [(name, n)]; Nothing -> []
  booleanMetric name key = case boolMaybe raw key of Just value -> [(name, if value then 1 else 0)]; Nothing -> []

metricName "setupMs" = "heuristic_setup_ms"
metricName "accountPrepareMs" = "account_prepare_ms"
metricName "targetPrepareMs" = "heuristic_prepare_ms"
metricName "forwardSearchMs" = "forward_search_ms"
metricName "reverseDijkstraMs" = "reverse_search_ms"
metricName "seedTableMs" = "seed_table_ms"
metricName "searchMs" = "search_ms"
metricName "totalMs" = "total_ms"
metricName name = camelToSnake name

camelToSnake = concatMap (\c -> if c >= 'A' && c <= 'Z' then ['_', toLowerAscii c] else [c])
toLowerAscii c = toEnum (fromEnum c + fromEnum 'a' - fromEnum 'A')

dimensions :: Value -> Map.Map String String
dimensions raw = Map.fromList
  [ ("allow_transports", if bool raw "allowTransports" False then "true" else "false")
  , ("heuristic_weight", show (numberDouble raw "heuristicWeight" 1))
  ]

status :: Value -> String
status raw = if bool raw "correct" True then "ok" else "incorrect"

validResult (raw, _) = text raw "expectation" "" `elem` ["positive", "negative"] && boolMaybe raw "reachable" /= Nothing && boolMaybe raw "oracleReachable" /= Nothing

text (Object o) key fallback = case KeyMap.lookup (Key.fromString key) o of Just (String v) -> Text.unpack v; _ -> fallback
text _ _ fallback = fallback

bool (Object o) key fallback = case KeyMap.lookup (Key.fromString key) o of Just (Bool v) -> v; _ -> fallback
bool _ _ fallback = fallback
boolMaybe :: Value -> String -> Maybe Bool
boolMaybe (Object o) key = case KeyMap.lookup (Key.fromString key) o of Just (Bool v) -> Just v; _ -> Nothing
boolMaybe _ _ = Nothing
number :: Value -> String -> Int -> Int
number raw key fallback = fromMaybe fallback (numberMaybe raw key)
numberMaybe :: Value -> String -> Maybe Int
numberMaybe (Object o) key = case KeyMap.lookup (Key.fromString key) o of Just (Number n) -> Just (round n); _ -> Nothing
numberMaybe _ _ = Nothing
firstNumberMaybe _ [] = Nothing
firstNumberMaybe raw (key:keys) = case numberMaybe raw key of Just n -> Just n; Nothing -> firstNumberMaybe raw keys
numberDouble :: Value -> String -> Double -> Double
numberDouble raw key fallback = fromMaybe fallback (numberDoubleMaybe raw key)
numberDoubleMaybe :: Value -> String -> Maybe Double
numberDoubleMaybe (Object o) key = case KeyMap.lookup (Key.fromString key) o of Just (Number n) -> Just (toRealFloat n); _ -> Nothing
numberDoubleMaybe _ _ = Nothing

routeCount rows = length (Map.keys (Map.fromList [((text (fst r) "routeId" "", text (fst r) "category" ""), ()) | r <- rows]))
caseCount rows = length (Map.keys (Map.fromList [((text (fst r) "routeId" "", text (fst r) "accountProfile" ""), ()) | r <- rows]))
caseCountFor expectation rows = length (Map.keys (Map.fromList [((text raw "routeId" "", text raw "accountProfile" ""), ()) | (raw, _) <- rows, text raw "expectation" "" == expectation]))
generatedRunId first = map clean (text first "generatedAt" "run") <> "-" <> take 8 (text first "gitCommit" "unknown")
 where clean ' ' = 'T'; clean ':' = '-'; clean c = c

gitHash input = takeWhile (/= '\n') <$> readProcess "git" ["hash-object", "--stdin"] (map (toEnum . fromEnum) (BS.unpack input))

clickhouse :: Options -> String -> LBS.ByteString -> IO String
clickhouse o query body = bracket (openBinaryTempFile "/tmp" "bench-import-") cleanup $ \(path, handle) -> do
  hClose handle
  LBS.writeFile path body
  readProcess "curl" ["-sS", "--fail", "-X", "POST", "--data-binary", "@" <> path, clickhouseUrl o <> "/?query=" <> urlEncode query] ""
 where cleanup (path, handle) = hClose handle >> removeFile path

sqlString = concatMap (\c -> if c == '\'' then "''" else [c])

urlEncode = concatMap encodeChar
 where
  encodeChar ' ' = "%20"
  encodeChar '\'' = "%27"
  encodeChar '(' = "%28"
  encodeChar ')' = "%29"
  encodeChar ',' = "%2C"
  encodeChar c = [c]

trim = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\n')

dieUsage = die "usage: bench-import [--corpus-dir DIR] [--run-id ID] [--notes TEXT] [--testbed ID] [--clickhouse-url URL] [--corpus PATH] [--exclusions PATH] [--profile-source PATH] [--sweep ID] results.jsonl"
die message = putStrLn message >> exitFailure
