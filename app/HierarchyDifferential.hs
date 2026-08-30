{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (filterM, forM, when)
import Data.Aeson (FromJSON(..), Value, encode, eitherDecode, object, withObject, (.:), (.=))
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, isEOF, stdout)
import Text.Printf (printf)

import ShortestPath.Exact.Hierarchical
  ( Hierarchical, QueryTimings(..), SearchCounters(..), buildHierarchical, buildHierarchicalWithRegionTable, findRouteProfiledWithOptions
  , hierarchicalRegionGraphSize
  )
import ShortestPath.Heuristic.Region (RegionTable, buildRegionGraph, buildRegionTable)
import ShortestPath.Exact.RawDijkstra (RawDijkstra(..))
import ShortestPath.Hierarchy.Partition
import ShortestPath.Hierarchy.Preprocess (preprocessHierarchy)
import ShortestPath.Hierarchy.Types (Hierarchy(..), LeafOverlay(..))
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

data NamedCase = NamedCase
  { caseName :: String
  , caseStart :: Tile
  , caseTarget :: Tile
  , caseEnabledTypes :: Set.Set String
  }
  deriving (Eq, Show)

data TestCase = TestCase
  { testLabel :: String
  , testQuery :: Query
  }
  deriving (Eq, Show)

data Point = Point
  { pointX :: Int
  , pointY :: Int
  , pointPlane :: Int
  }

instance FromJSON Point where
  parseJSON = withObject "coordinate" $ \value ->
    Point <$> value .: "x" <*> value .: "y" <*> value .: "plane"

data ServeRequest = ServeRequest
  { requestId :: Int
  , requestStart :: Point
  , requestTarget :: Point
  , requestAllowTransports :: Bool
  , requestIncludeExpandedTiles :: Bool
  , requestUseHeuristic :: Bool
  }

instance FromJSON ServeRequest where
  parseJSON = withObject "route request" $ \value ->
    ServeRequest
      <$> value .: "id"
      <*> value .: "start"
      <*> value .: "target"
      <*> value .: "allowTransports"
      <*> value .: "includeExpandedTiles"
      <*> value .: "useHeuristic"

data HierarchyCache = HierarchyCache Word64 Hierarchy
  deriving stock (Generic)
  deriving anyclass (Binary)

data RegionTableCache = RegionTableCache Word64 RegionTable
  deriving stock (Generic)
  deriving anyclass (Binary)

main :: IO ()
main = do
  command <- parseCommand =<< getArgs
  world <- timedPhase "load world" (loadWorld defaultSourcePaths)
  hierarchy <- loadOrBuildHierarchy world
  case command of
    GenerateRegionTable -> do
      regionGraph <- timedPhase "build detailed optimistic terminal graph" $ do
        let value = buildRegionGraph world hierarchy
        _ <- evaluate value
        pure value
      table <- timedPhase "generate region lower-bound table" (buildRegionTable regionGraph)
      timedPhase "write region lower-bound table" (encodeFile regionTablePath (RegionTableCache regionTableVersion table))
    _ -> do
      table <- loadFreshRegionTable
      hierarchical <- timedPhase "load routing heuristic" $ do
        let value = maybe (buildHierarchical world hierarchy) (buildHierarchicalWithRegionTable world hierarchy) table
            (nodes, edges) = hierarchicalRegionGraphSize value
        _ <- evaluate (nodes + edges)
        printf "routing heuristic: %d regions/nodes, %d edges/table entries\n" nodes edges
        pure value
      runCommand command world hierarchy hierarchical

runCommand :: Command -> World -> Hierarchy -> Hierarchical -> IO ()
runCommand command world hierarchy hierarchical = do
  let partition = hierarchyPartition hierarchy
  case command of
    Serve -> serveLoop hierarchical
    Run mode writeRoutes -> do
      let cases = selectCases mode partition
          raw = RawDijkstra world
      printf "hierarchy differential: running %d cases (%s)\n" (length cases) (show mode)
      results <- forM (zip [1 :: Int ..] cases) $ \(number, testCase) -> do
        printf "route %d/%d: %s\n" number (length cases) (testLabel testCase)
        hFlush stdout
        (flat, abstract) <- compareRoute partition raw hierarchical testCase
        pure (testCase, flat, abstract)
      when writeRoutes (writeCorpus results)
      putStrLn "hierarchy differential: pass"
    GenerateRegionTable -> pure ()

data Mode = Smoke | All
  deriving (Eq, Show)

data Command = Run Mode Bool | Serve | GenerateRegionTable
  deriving (Eq, Show)

parseCommand :: [String] -> IO Command
parseCommand [] = pure (Run Smoke False)
parseCommand ["smoke"] = pure (Run Smoke False)
parseCommand ["all"] = pure (Run All False)
parseCommand [mode, "--write-routes"]
  | mode == "smoke" = pure (Run Smoke True)
  | mode == "all" = pure (Run All True)
parseCommand ["serve"] = pure Serve
parseCommand ["generate-region-table"] = pure GenerateRegionTable
parseCommand _ = do
  putStrLn "usage: runghc app/HierarchyDifferential.hs [smoke|all [--write-routes]|serve|generate-region-table]"
  exitFailure

selectCases :: Mode -> Partition -> [TestCase]
selectCases mode partition =
  let named = map namedTest namedCases
      generated = generatedCases partition
   in case mode of
        Smoke -> take 13 named <> take 24 generated
        All -> named <> generated

namedTest :: NamedCase -> TestCase
namedTest named =
  TestCase
    { testLabel = "tooling coordinates only: " <> caseName named
    , testQuery = (defaultQuery (caseStart named) (caseTarget named))
        { enabledTransportTypes = caseEnabledTypes named }
    }

-- Coordinates are copied from ../shortest-path-tooling/.../dashboard/unit-tests.csv.
-- Java expected_length, inventory, requirements, wilderness, and config are deliberately ignored.
namedCases :: [NamedCase]
namedCases =
  [ named "Draynor Manor stepping stones vs combat bracelet" (t 3149 3363) (t 3154 3363) ["AGILITY_SHORTCUT"]
  , named "Keldagrim east -> west via other plane" (t 2894 10199) (t 2864 10199) ["TRANSPORT"]
  , named "Keldagrim west -> east via other plane" (t 2864 10199) (t 2894 10199) ["TRANSPORT"]
  , named "Deep wilderness -> Grand Exchange with no teleports" (t 3340 3828) (t 3158 3509) ["AGILITY_SHORTCUT"]
  , named "Wizards' Guild -> Edgeville with no items and wilderness allowed" (t 2485 3080) (t 3087 3492) ["TELEPORTATION_LEVER"]
  , named "Catherby charter tile reuse -> bank -> Musa Point" (t 2792 3414) (t 2954 3158) ["CHARTER_SHIP"]
  , named "Castle Wars -> AKQ" (t 2442 3083) (t 2324 3619) ["FAIRY_RING"]
  , named "Great Conch -> McGrubor's Wood" (t 3180 2419) (t 2652 3485) ["FAIRY_RING", "AGILITY_SHORTCUT"]
  , named "Varrock centre -> Cowbell amulet destination" (t 3213 3424) (t 3259 3277) ["TELEPORTATION_ITEM"]
  , named "Lovakengj reverse minecart" (t 1415 3577) (t 1670 3833) ["MINECART"]
  , named "Varrock teleport" (t 3223 3424) (t 3213 3424) ["TELEPORTATION_SPELL"]
  , named "Castle Wars -> Grand Exchange" (t 2442 3096) (t 3162 3489) ["FAIRY_RING"]
  , named "Al Kharid mine -> AKQ" (t 3298 3290) (t 2319 3619) ["FAIRY_RING"]
  , named "Banked Dramen staff -> McGrubor's Wood" (t 3134 3503) (t 2652 3485) ["FAIRY_RING"]
  ]
 where
  t x y = packTile x y 0
  named name start target types = NamedCase name start target (Set.fromList types)

generatedCases :: Partition -> [TestCase]
generatedCases partition =
  concatMap componentCases (Map.toAscList grouped)
  <> [ TestCase
         { testLabel = "generated cross-leaf walking: " <> show left <> " -> " <> show right
         , testQuery = (defaultQuery leftTile rightTile) {allowTransports = False}
         }
     | ((left, leftTiles), (right, rightTiles)) <- adjacentLeaves
     , Just leftTile <- [firstTile leftTiles]
     , Just rightTile <- [firstTile rightTiles]
     ]
 where
  grouped = Map.fromListWith (<>)
    [ (component, [(leaf, tiles)])
    | (leaf, tiles) <- Map.toAscList (leafTileSets partition)
    , let LeafId component _ = leaf
    ]
  leaves = Map.toAscList (leafTileSets partition)
  adjacentLeaves = zip leaves (drop 1 leaves)
  firstTile tiles = Tile . fst <$> IntSet.minView tiles

  componentCases (component, leavesInComponent) =
    [ TestCase
        { testLabel = "generated component " <> show component <> " walking case " <> show number
        , testQuery = (defaultQuery a b) {allowTransports = False}
        }
    | (number, (a, b)) <- zip [1 :: Int ..] (take 10 (zip points (drop 1 (cycle points))))
    ]
   where
    points = concatMap endpoints leavesInComponent
    endpoints (_, tiles) = case (IntSet.minView tiles, IntSet.maxView tiles) of
      (Just (firstPacked, _), Just (lastPacked, _)) -> [Tile firstPacked, Tile lastPacked]
      _ -> []

compareRoute :: Partition -> RawDijkstra -> Hierarchical -> TestCase -> IO (Route, Route)
compareRoute partition raw hierarchical testCase = do
  let query = testQuery testCase
      flat = findRoute raw query
      abstract = findRoute hierarchical query
  when (routeCost flat /= routeCost abstract) $ do
    putStrLn "DIFFERENTIAL MISMATCH"
    putStrLn ("case: " <> testLabel testCase)
    putStrLn ("source: " <> coordinateText (queryStart query))
    putStrLn ("target: " <> coordinateText (queryTarget query))
    putStrLn ("query: " <> show query)
    putStrLn ("raw cost: " <> show (routeCost flat))
    putStrLn ("raw route: " <> show (routeSteps flat))
    putStrLn ("hierarchical cost: " <> show (routeCost abstract))
    putStrLn ("hierarchical route: " <> show (routeSteps abstract))
    putStrLn ("raw regions: " <> show (regionSequence partition query flat))
    putStrLn ("hierarchical regions: " <> show (regionSequence partition query abstract))
    exitFailure
  pure (flat, abstract)

writeCorpus :: [(TestCase, Route, Route)] -> IO ()
writeCorpus results = do
  createDirectoryIfMissing True "out"
  LBS.writeFile "out/hierarchy-test-routes.json" (encode (map resultJson results))
  putStrLn "wrote out/hierarchy-test-routes.json"
 where
  resultJson (testCase, flat, abstract) =
    object
      [ "name" .= testLabel testCase
      , "source" .= coordinateText (queryStart (testQuery testCase))
      , "target" .= coordinateText (queryTarget (testQuery testCase))
      , "rawCost" .= routeCost flat
      , "hierarchicalCost" .= routeCost abstract
      , "path" .= routeStepsJson (routeSteps abstract)
      ]

routeStepsJson :: [RouteStep] -> [Value]
routeStepsJson = map stepJson
 where
  stepJson (Walk tile) = object ["kind" .= ("walk" :: String), "coordinate" .= coordinateText tile]
  stepJson (UseTransport label tile) = object ["kind" .= ("transport" :: String), "label" .= label, "coordinate" .= coordinateText tile]

serveLoop :: Hierarchical -> IO ()
serveLoop hierarchical = do
  LBS.putStrLn (encode (object ["ready" .= True]))
  hFlush stdout
  loop
 where
  loop = do
    done <- isEOF
    when (not done) $ do
      line <- BS.getLine
      response <- serveRequest hierarchical (LBS.fromStrict line)
      LBS.putStrLn (encode response)
      hFlush stdout
      loop

serveRequest :: Hierarchical -> LBS.ByteString -> IO Value
serveRequest hierarchical line =
  case eitherDecode line of
    Left message -> pure (object ["ok" .= False, "error" .= ("invalid JSON request: " <> message)])
    Right request
      | not (validPoint (requestStart request)) -> invalid request "start coordinate is outside 0..32767 or has an invalid plane"
      | not (validPoint (requestTarget request)) -> invalid request "target coordinate is outside 0..32767 or has an invalid plane"
      | otherwise -> do
          let query = (defaultQuery (pointTile (requestStart request)) (pointTile (requestTarget request)))
                { allowTransports = requestAllowTransports request }
          (route, timings, expandedTiles, heuristicRegions, heuristicTiles) <- findRouteProfiledWithOptions
            (requestIncludeExpandedTiles request)
            (requestUseHeuristic request)
            hierarchical
            query
          pure (object
            [ "id" .= requestId request
            , "ok" .= True
            , "cost" .= routeCost route
            , "expandedNodes" .= routeExpandedNodes route
            , "path" .= routeStepsJson (routeSteps route)
            , "expandedTiles" .= map coordinateText expandedTiles
            , "heuristicRegions" .= map heuristicRegionJson heuristicRegions
            , "heuristicTiles" .= map heuristicTileJson heuristicTiles
            , "timings" .= timingsJson timings
            ])
 where
  invalid :: ServeRequest -> String -> IO Value
  invalid request message = pure (object ["id" .= requestId request, "ok" .= False, "error" .= message])
  pointTile point = packTile (pointX point) (pointY point) (pointPlane point)
  validPoint point = pointX point >= 0 && pointX point <= 32767
    && pointY point >= 0 && pointY point <= 32767
    && pointPlane point >= 0 && pointPlane point <= 3
  heuristicRegionJson (LeafId component region, value) = object
    [ "component" .= component
    , "region" .= region
    , "value" .= value
    ]
  heuristicTileJson (tile, value) =
    let (x, y, plane) = unpackTile tile
     in object ["x" .= x, "y" .= y, "plane" .= plane, "value" .= value]

timingsJson :: QueryTimings -> Value
timingsJson timings = object
  [ "sourceAttachmentMs" .= sourceAttachmentMilliseconds timings
  , "targetAttachmentMs" .= targetAttachmentMilliseconds timings
  , "abstractSearchMs" .= abstractSearchMilliseconds timings
  , "reconstructionMs" .= reconstructionMilliseconds timings
  , "totalMs" .= totalMilliseconds timings
  , "heuristicMs" .= heuristicMilliseconds timings
  , "search" .= searchCountersJson (querySearchCounters timings)
  ]

searchCountersJson :: SearchCounters -> Value
searchCountersJson counters = object
  [ "queuePops" .= searchQueuePops counters
  , "stalePops" .= searchStalePops counters
  , "edgesConsidered" .= searchEdgesConsidered counters
  , "successfulRelaxations" .= searchSuccessfulRelaxations counters
  , "sourceEdges" .= searchSourceEdges counters
  , "targetEdges" .= searchTargetEdges counters
  , "metricEdges" .= searchMetricEdges counters
  , "separatorEdges" .= searchSeparatorEdges counters
  , "localTransportEdges" .= searchLocalTransportEdges counters
  , "globalEntryEdges" .= searchGlobalEntryEdges counters
  , "globalTeleportEdges" .= searchGlobalTeleportEdges counters
  , "bankEdges" .= searchBankEdges counters
  , "heuristicLookups" .= searchHeuristicLookups counters
  ]

timedPhase :: String -> IO a -> IO a
timedPhase label action = do
  putStrLn ("hierarchy differential: " <> label)
  hFlush stdout
  started <- getMonotonicTimeNSec
  result <- action
  finished <- getMonotonicTimeNSec
  printf "hierarchy differential: %s completed in %.1f ms\n" label (milliseconds started finished)
  hFlush stdout
  pure result

milliseconds :: Word64 -> Word64 -> Double
milliseconds started finished = fromIntegral (finished - started) / 1000000

loadOrBuildHierarchy :: World -> IO Hierarchy
loadOrBuildHierarchy world = do
  fresh <- cacheIsFresh
  cached <- if fresh then loadCache else pure Nothing
  case cached of
    Just hierarchy -> timedPhase "force cached hierarchy" (forceHierarchy hierarchy)
    Nothing -> do
      partition <- timedPhase "load saved KaHIP assignments" (loadPartition world partitionPath)
      hierarchy <- timedPhase "preprocess hierarchy" (preprocessHierarchy partition world)
      createDirectoryIfMissing True "out"
      timedPhase "write hierarchy cache" (encodeFile cachePath (HierarchyCache cacheVersion hierarchy))
      pure hierarchy

loadCache :: IO (Maybe Hierarchy)
loadCache = timedPhase "load hierarchy cache" $ do
  decoded <- decodeFileOrFail cachePath
  case decoded of
    Right (HierarchyCache version hierarchy) | version == cacheVersion -> pure (Just hierarchy)
    Right _ -> putStrLn "hierarchy cache version mismatch; rebuilding" >> pure Nothing
    Left (_, message) -> putStrLn ("hierarchy cache decode failed; rebuilding: " <> message) >> pure Nothing

forceHierarchy :: Hierarchy -> IO Hierarchy
forceHierarchy hierarchy = do
  let overlaySize = Map.foldl' (\total overlay -> total + overlayEntries overlay) 0 (leafOverlays hierarchy)
      size = IntMap.size (tileClasses (hierarchyPartition hierarchy))
        + Map.size (leafTileSets (hierarchyPartition hierarchy))
        + Map.size (terminalLeaf hierarchy)
        + overlaySize
  _ <- evaluate size
  pure hierarchy
 where
  overlayEntries overlay =
    Map.size (leafTerminals overlay)
      + Map.size (leafDistances overlay)
      + Map.foldl' (\total adjacent -> total + Map.size adjacent) 0 (leafTerminalAdjacency overlay)

cacheIsFresh :: IO Bool
cacheIsFresh = do
  exists <- doesFileExist cachePath
  if not exists
    then pure False
    else do
      inputs <- concat <$> mapM filesBelow cacheInputRoots
      cacheTime <- getModificationTime cachePath
      and <$> mapM (fmap (<= cacheTime) . getModificationTime) inputs

loadFreshRegionTable :: IO (Maybe RegionTable)
loadFreshRegionTable = do
  fresh <- regionTableIsFresh
  if not fresh
    then putStrLn "region lower-bound table missing or stale; using per-query detailed heuristic" >> pure Nothing
    else do
      decoded <- decodeFileOrFail regionTablePath
      case decoded of
        Right (RegionTableCache version table) | version == regionTableVersion -> do
          putStrLn ("loaded region lower-bound table: " <> regionTablePath)
          pure (Just table)
        Right _ -> putStrLn "region lower-bound table version mismatch; using detailed heuristic" >> pure Nothing
        Left (_, message) -> putStrLn ("region lower-bound table decode failed: " <> message) >> pure Nothing

regionTableIsFresh :: IO Bool
regionTableIsFresh = do
  exists <- doesFileExist regionTablePath
  if not exists
    then pure False
    else do
      tableTime <- getModificationTime regionTablePath
      inputs <- mapM getModificationTime [cachePath, "src/ShortestPath/Heuristic/Region.hs"]
      pure (all (<= tableTime) inputs)

filesBelow :: FilePath -> IO [FilePath]
filesBelow path = do
  directory <- doesDirectoryExist path
  if not directory
    then pure [path]
    else do
      entries <- map (path </>) <$> listDirectory path
      files <- filterM doesFileExist entries
      directories <- filterM doesDirectoryExist entries
      nested <- concat <$> mapM filesBelow directories
      pure (files <> nested)

cacheVersion :: Word64
cacheVersion = 2

regionTableVersion :: Word64
regionTableVersion = 1

cachePath, partitionPath, regionTablePath :: FilePath
cachePath = "out/hierarchy-cache.bin"
partitionPath = "out/metis/kahip-partitions.csv"
regionTablePath = "out/region-lower-bounds.bin"

cacheInputRoots :: [FilePath]
cacheInputRoots =
  [ partitionPath
  , resourcesDir defaultSourcePaths
  , "src/ShortestPath/Hierarchy"
  , "src/ShortestPath/Tile.hs"
  , "src/ShortestPath/Transport.hs"
  , "src/ShortestPath/World.hs"
  ]

regionSequence :: Partition -> Query -> Route -> [String]
regionSequence partition query route =
  stableRuns (map classify (queryStart query : routeTiles route))
 where
  routeTiles = map stepTile . routeSteps
  stepTile (Walk tile) = tile
  stepTile (UseTransport _ tile) = tile
  classify tile =
    case IntMap.lookup (unTile tile) (tileClasses partition) of
      Nothing -> "unindexed:" <> coordinateText tile
      Just tileClass -> show tileClass

stableRuns :: Eq a => [a] -> [a]
stableRuns [] = []
stableRuns (firstValue : rest) = firstValue : go firstValue rest
 where
  go _ [] = []
  go previous (nextValue : values)
    | previous == nextValue = go previous values
    | otherwise = nextValue : go nextValue values
