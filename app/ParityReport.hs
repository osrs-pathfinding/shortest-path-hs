{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (nub, sort, sortOn)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word32)
import System.Environment (getArgs)
import System.Exit (die)

import ShortestPath.BenchmarkCorpus
import ShortestPath.BenchmarkProfiles
import ShortestPath.Account (RequirementMode(..))
import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.Cache (loadOrBuildTileAStar)
import ShortestPath.Exact.TileAStar.Heuristic
import ShortestPath.Exact.TileAStar.RelaxedGraph
import ShortestPath.Exact.TileAStar.ReverseSearch
import ShortestPath.Exact.TileAStar.StaticArtifact
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

-- T3.5's comparison format is deliberately boring: one sorted, language-neutral
-- record per semantic fact. It is a diagnostic boundary, not a runtime API.
main :: IO ()
main = do
  output <- parseOutput =<< getArgs
  corpus <- discoverCorpusDir Nothing
  profiles <- loadBenchmarkProfiles corpus
  routes <- loadBenchmarkRoutes corpus
  world <- loadWorld defaultSourcePaths
  astar <- loadOrBuildTileAStar world
  artifact <- either fail pure (routingStaticV1 astar)
  let records = staticRecords astar artifact
  writeRecords output (records <> concatMap (caseRecords astar routes profiles) matrix)

matrix :: [(String, String)]
matrix =
  [ ("walking-0002", "early")
  , ("transport-heavy-0027", "maxed")
  , ("transport-heavy-0051", "end")
  , ("regression-0016", "maxed")
  , ("transport-heavy-0007", "early")
  , ("gps-natural-0012", "early")
  ]

parseOutput :: [String] -> IO (Maybe FilePath)
parseOutput [] = pure Nothing
parseOutput ["--output", path] = pure (Just path)
parseOutput _ = die "usage: parity-report [--output PATH]"

writeRecords :: Maybe FilePath -> [String] -> IO ()
writeRecords Nothing rows = mapM_ putStrLn (sort rows)
writeRecords (Just path) rows = writeFile path (unlines (sort rows))

caseRecords :: TileAStar -> [BenchmarkRoute] -> BenchmarkProfiles -> (String, String) -> [String]
caseRecords astar routes profiles (routeId, profileName) =
  case (findRoute routeId routes, benchmarkAccountFrom profiles profileName) of
    (Just route, Just account) ->
      let start = routeTile (routeStart route)
          target = routeTile (routeTarget route)
          query = (defaultQuery start target)
            { allowTransports = routeAllowTransports route
            , requirementMode = ConfiguredRequirements account
            , queryNowMinutes = benchmarkNowMinutesFrom profiles
            }
          compiled = compileRoutingAccount astar (routingOptionsFromQuery query)
          graph = compiledSiteGraph compiled
          overlay = targetOverlay astar compiled target
          labels = reverseDijkstraUncounted graph overlay
          sparseLabels = Vector.take (Vector.length labels)
            (halveDistances (manhattanDistances (reverseDijkstraManhattanUncounted graph overlay)))
          heuristic = heuristicFromDistances
            (topologyRoutingComponents (tileTopology astar)) graph overlay labels 0 0 emptyReverseCounters
       in if sparseLabels /= labels
            then error ("sparse/clique label mismatch " <> routeId <> "/" <> profileName)
            else caseRecordsFor (tileTopology astar) route profileName query compiled graph overlay labels heuristic
    _ -> error ("missing parity matrix fixture " <> routeId <> "/" <> profileName)

caseRecordsFor :: WorldTopology -> BenchmarkRoute -> String -> Query -> CompiledRoutingAccount -> SiteGraph
  -> TargetOverlay -> Vector.Vector Int -> Heuristic -> [String]
caseRecordsFor topology route profileName query compiled graph overlay labels heuristic =
  [ record ["case", caseId, profileName, u32Int (unTile (queryStart query)), u32Int (unTile (queryTarget query))]
  , record ["account", caseId, profileName, bool (compiledAllowTransports compiled)
      , bool (compiledBankPathEnabled compiled)]
  ]
  <> availabilityRecords caseId profileName compiled
  <> nodeRecords caseId profileName graph
  <> edgeRecords caseId profileName graph
  <> targetRecords caseId profileName overlay
  <> labelRecords caseId profileName labels
  <> seedRecords caseId profileName (heuristicSeeds heuristic)
  <> probeRecords caseId profileName topology heuristic query graph overlay
 where
  caseId = maybe (routeName route) id (routeId route)

staticRecords :: TileAStar -> RoutingStaticV1 -> [String]
staticRecords astar artifact =
  [ record ["static-meta", show (Vector.length (artifactSearchTiles artifact))
      , show (Vector.length (artifactSiteTiles artifact))
      , show (artifactRoutingComponentCount artifact)
      , show (Vector.length (artifactReachableBankTiles artifact))
      , show (Vector.length (artifactCrossingFromSite artifact))
      , show (artifactSparseOriginalCount artifact)
      , show (artifactSparseVertexCount artifact)
      , show (artifactSparseSteinerCount artifact)
      , show (artifactSparseUndirectedEdgeCount artifact)
      , show (artifactSparseAdjacencyCount artifact)
      , effectiveCollisionFingerprint (worldCollision (topologyWorld (tileTopology astar))) artifact]
  ]
  <> [ record ["static-search", show ix, u32 (artifactSearchTiles artifact Vector.! ix)
      , show (artifactSearchComponents artifact Vector.! ix)
      , show (artifactWalkingMasks artifact Vector.! ix)
      , show (artifactNorthNodes artifact Vector.! ix)
      , show (artifactSouthNodes artifact Vector.! ix)]
     | ix <- sampleIndices (Vector.length (artifactSearchTiles artifact))]
  <> [ record ["static-site", show ix, u32 (artifactSiteTiles artifact Vector.! ix)
      , csvInts (staticSiteComponents artifact ix)]
     | ix <- [0 .. Vector.length (artifactSiteTiles artifact) - 1]]
  <> [ record ["static-bank", u32 (artifactReachableBankTiles artifact Vector.! ix)]
     | ix <- [0 .. Vector.length (artifactReachableBankTiles artifact) - 1]]
  <> [ record ["static-crossing", show fromTile, show toTile, show cost]
     | (fromSite, toSite, cost) <- crossingRows artifact
     , let fromTile = artifactSiteTiles artifact Vector.! fromSite
     , let toTile = artifactSiteTiles artifact Vector.! toSite]
  <> [ record ["static-sparse", show ix
      , show (sparseSource artifact ix), show (artifactSparseDestinations artifact Vector.! ix)
      , show (artifactSparseWeights artifact Vector.! ix)]
     | ix <- sampleIndices (Vector.length (artifactSparseDestinations artifact))]

staticSiteComponents :: RoutingStaticV1 -> Int -> [Int]
staticSiteComponents artifact ix =
  [ fromIntegral (artifactSiteComponentIds artifact Vector.! p)
  | p <- [fromIntegral (artifactSiteComponentOffsets artifact Vector.! ix)
        .. fromIntegral (artifactSiteComponentOffsets artifact Vector.! (ix + 1)) - 1]]

crossingRows :: RoutingStaticV1 -> [(Int, Int, Int)]
crossingRows artifact =
  [ ( fromIntegral (artifactCrossingFromSite artifact Vector.! ix)
    , fromIntegral (artifactCrossingToSite artifact Vector.! ix)
    , fromIntegral (artifactCrossingCosts artifact Vector.! ix))
  | ix <- [0 .. Vector.length (artifactCrossingFromSite artifact) - 1]]

sparseSource :: RoutingStaticV1 -> Int -> Int
sparseSource artifact ix =
  go 0
 where
  offsets = artifactSparseOffsets artifact
  go source
    | source + 1 >= Vector.length offsets = -1
    | fromIntegral (offsets Vector.! (source + 1)) > ix = source
    | otherwise = go (source + 1)

availabilityRecords :: String -> String -> CompiledRoutingAccount -> [String]
availabilityRecords routeId profileName compiled =
  concatMap viewRows views
 where
  availability = compiledTransportAvailability compiled
  penalties = compiledTransportPenalties compiled
  views =
    [ ("local", False, concatMap (localRows False) (Map.toAscList (carriedLocalTransports availability)))
    , ("local", True, concatMap (localRows True) (Map.toAscList (bankedLocalTransports availability)))
    , ("global", False, globalRows False (carriedGlobalTransports availability))
    , ("global", True, globalRows True (bankedGlobalTransports availability))
    , ("wilderness", False, globalRows False (carriedWildernessGlobalTransports availability))
    , ("wilderness", True, globalRows True (bankedWildernessGlobalTransports availability))
    ]
  viewRows (name, banked, rows) =
    [ record ["availability", routeId, profileName, name, bool banked
        , if name == "local" then u32Int originTile else show originTile
        , u32Int (unTile destinationTile), show cost
        , transportType transport, show (maybe (-1) id (maxWildernessLevel transport))]
    | (originTile, destinationTile, cost, transport) <- sortOn availabilityKey rows]
  localRows banked (originTile, transports) =
    [ (unTile originTile, destinationTile, cost transport, transport)
    | transport <- transports, Just destinationTile <- [destination transport]]
   where
    cost transport = duration transport + Map.findWithDefault 0 (transportType transport) penalties
  globalRows _ transports =
    [ (-1, destinationTile, cost transport, transport)
    | transport <- transports, Just destinationTile <- [destination transport]]
   where
    cost transport = duration transport + Map.findWithDefault 0 (transportType transport) penalties
  availabilityKey (originTile, destinationTile, cost, transport) =
    (originTile, unTile destinationTile, cost, transportType transport, maybe (-1) id (maxWildernessLevel transport))

nodeRecords :: String -> String -> SiteGraph -> [String]
nodeRecords routeId profileName graph =
  [ record ["node", routeId, profileName, show ix, u32Int (siteTiles graph Vector.! ix)]
  | ix <- [0 .. siteStaticCount graph - 1]]
  <> [record ["node", routeId, profileName, show (siteStaticCount graph + ix), "hub"]
     | ix <- [0 .. Boxed.length (siteAbstractNodes graph) - 1]]

edgeRecords :: String -> String -> SiteGraph -> [String]
edgeRecords routeId profileName graph = sort
  [ record ["edge", routeId, profileName, show from, show to, show cost, bool starts]
  | to <- [0 .. Boxed.length (siteReverseEdges graph) - 1]
  , (from, cost, starts) <- Vector.toList (siteReverseEdges graph Boxed.! to)]

targetRecords :: String -> String -> TargetOverlay -> [String]
targetRecords routeId profileName overlay =
  [ record ["target", routeId, profileName, show (targetSite overlay), bool (targetSynthetic overlay)
      , u32Int (targetPacked overlay), csvInts (Vector.toList (targetComponents overlay))]
  ]
  <> [record ["target-attachment", routeId, profileName, show site, show cost]
     | (site, cost) <- Vector.toList (targetAttachmentSites overlay)]

labelRecords :: String -> String -> Vector.Vector Int -> [String]
labelRecords routeId profileName labels =
  [ record ["label", routeId, profileName, show state, show value]
  | (state, value) <- Vector.toList (Vector.indexed labels), value /= maxBound]

seedRecords :: String -> String -> Boxed.Vector (Vector.Vector (Int, Int)) -> [String]
seedRecords routeId profileName seeds =
  [ record ["seed", routeId, profileName, show (key `div` 2), bool (odd key), u32Int tile, show label]
  | (key, entries) <- Boxed.toList (Boxed.indexed seeds)
  , (tile, label) <- sort (Vector.toList entries)]

probeRecords :: String -> String -> WorldTopology -> Heuristic -> Query -> SiteGraph -> TargetOverlay -> [String]
probeRecords routeId profileName topology heuristic query graph overlay =
  [ record ["probe", routeId, profileName, u32Int tile, bool banked, maybe showValue show (heuristicAt topology heuristic (Tile tile) banked)]
  | tile <- probeTiles query graph overlay
  , banked <- [False, True]
  ]
 where
  showValue = show (maxBound :: Int32)

probeTiles :: Query -> SiteGraph -> TargetOverlay -> [Int]
probeTiles query graph overlay = take 32 (sort (nub
  ( unTile (queryStart query) : unTile (queryTarget query)
  : targetPacked overlay : Vector.toList (Vector.take 4 (siteTiles graph))
  <> Vector.toList (Vector.take 4 (Vector.drop (max 0 (Vector.length (siteTiles graph) - 4)) (siteTiles graph)))
  )))

findRoute :: String -> [BenchmarkRoute] -> Maybe BenchmarkRoute
findRoute wanted = go
 where
  go [] = Nothing
  go (route:rest)
    | routeId route == Just wanted = Just route
    | otherwise = go rest

routeTile :: [Int] -> Tile
routeTile [x, y, plane] = packTile x y plane
routeTile value = error ("invalid route point " <> show value)

sampleIndices :: Int -> [Int]
sampleIndices count = nub [ix | ix <- [0, count `div` 3, count `div` 2, count - 1], ix >= 0, ix < count]

record :: [String] -> String
record = foldr1 (<>) . map (<> "|")

u32 :: Word32 -> String
u32 = show

u32Int :: Int -> String
u32Int value = show (fromIntegral value :: Word32)

csvInts :: [Int] -> String
csvInts [] = "-"
csvInts values = foldr1 (<>) (map showWithComma (zip values (True : repeat False)))
 where
  showWithComma (value, first) = (if first then "" else ",") <> show value

bool :: Bool -> String
bool True = "1"
bool False = "0"
