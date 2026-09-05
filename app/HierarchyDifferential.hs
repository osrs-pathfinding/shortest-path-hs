{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (filterM, forM, when)
import Data.Aeson (FromJSON(..), Value, encode, eitherDecode, object, withObject, (.:), (.:?), (.=), (.!=))
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Ord (Down(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, isEOF, stdout)
import Data.List (sortOn)
import Text.Printf (printf)

import ShortestPath.Exact.Hierarchical
  ( Hierarchical, QueryTimings(..), SearchCounters(..), buildHierarchical, buildHierarchicalWithRegionTable, findRouteProfiledWithOptions
  , hierarchicalRegionGraphSize
  )
import ShortestPath.Exact.TileAStar
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
  , requestHeuristicWeight :: Double
  , requestFinder :: Maybe String
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
      <*> value .:? "heuristicWeight" .!= 1
      <*> value .:? "finder"

data HierarchyCache = HierarchyCache Word64 Hierarchy
  deriving stock (Generic)
  deriving anyclass (Binary)

data RegionTableCache = RegionTableCache Word64 RegionTable
  deriving stock (Generic)
  deriving anyclass (Binary)

data TileComponentCache = TileComponentCache Word64 NaturalComponents TileStatic
  deriving stock (Generic)
  deriving anyclass (Binary)

main :: IO ()
main = do
  command <- parseCommand =<< getArgs
  world <- timedPhase "load world" (loadWorld defaultSourcePaths)
  case command of
    ServeDirect -> do
      tileAStar <- loadOrBuildTileAStar world
      serveLoop world tileAStar Nothing
    ComponentTransformReport -> do
      tileAStar <- loadOrBuildTileAStar world
      writeComponentTransformReport tileAStar
    TileStaticReport -> do
      tileAStar <- loadOrBuildTileAStar world
      writeTileStaticReport tileAStar
    GenerateRegionTable -> do
      hierarchy <- loadOrBuildHierarchy world
      regionGraph <- timedPhase "build detailed optimistic terminal graph" $ do
        let value = buildRegionGraph world hierarchy
        _ <- evaluate value
        pure value
      table <- timedPhase "generate region lower-bound table" (buildRegionTable regionGraph)
      timedPhase "write region lower-bound table" (encodeFile regionTablePath (RegionTableCache regionTableVersion table))
    _ -> do
      hierarchy <- loadOrBuildHierarchy world
      table <- loadFreshRegionTable
      hierarchical <- timedPhase "load routing heuristic" $ do
        let value = maybe (buildHierarchical world hierarchy) (buildHierarchicalWithRegionTable world hierarchy) table
            (nodes, edges) = hierarchicalRegionGraphSize value
        _ <- evaluate (nodes + edges)
        printf "routing heuristic: %d regions/nodes, %d edges/table entries\n" nodes edges
        pure value
      tileAStar <- loadOrBuildTileAStar world
      runCommand command world hierarchy tileAStar hierarchical

runCommand :: Command -> World -> Hierarchy -> TileAStar -> Hierarchical -> IO ()
runCommand command world hierarchy tileAStar hierarchical = do
  let partition = hierarchyPartition hierarchy
  case command of
    Serve -> serveLoop world tileAStar (Just hierarchical)
    Run mode writeRoutes -> do
      let cases = selectCases mode partition
          raw = RawDijkstra world
      printf "hierarchy differential: running %d cases (%s)\n" (length cases) (show mode)
      results <- forM (zip [1 :: Int ..] cases) $ \(number, testCase) -> do
        printf "route %d/%d: %s\n" number (length cases) (testLabel testCase)
        hFlush stdout
        (flat, tile, abstract) <- compareRoute partition raw tileAStar hierarchical testCase
        pure (testCase, flat, tile, abstract)
      when writeRoutes (writeCorpus results)
      putStrLn "hierarchy differential: pass"
    ServeDirect -> pure ()
    GenerateRegionTable -> pure ()
    ComponentTransformReport -> pure ()
    TileStaticReport -> pure ()

data Mode = Smoke | All
  deriving (Eq, Show)

data Command = Run Mode Bool | Serve | ServeDirect | GenerateRegionTable | ComponentTransformReport | TileStaticReport
  deriving (Eq, Show)

parseCommand :: [String] -> IO Command
parseCommand [] = pure (Run Smoke False)
parseCommand ["smoke"] = pure (Run Smoke False)
parseCommand ["all"] = pure (Run All False)
parseCommand [mode, "--write-routes"]
  | mode == "smoke" = pure (Run Smoke True)
  | mode == "all" = pure (Run All True)
parseCommand ["serve"] = pure Serve
parseCommand ["serve-direct"] = pure ServeDirect
parseCommand ["generate-region-table"] = pure GenerateRegionTable
parseCommand ["component-transform-report"] = pure ComponentTransformReport
parseCommand ["tile-static-report"] = pure TileStaticReport
parseCommand _ = do
  putStrLn "usage: runghc app/HierarchyDifferential.hs [smoke|all [--write-routes]|serve|serve-direct|generate-region-table|component-transform-report|tile-static-report]"
  exitFailure

selectCases :: Mode -> Partition -> [TestCase]
selectCases mode partition =
  let named = map namedTest namedCases
      generated = generatedCases partition
   in case mode of
        Smoke -> named <> take 24 generated
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
  , named "Varrock tablet" (t 3223 3424) (t 3213 3424) ["TELEPORTATION_ITEM"]
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

compareRoute :: Partition -> RawDijkstra -> TileAStar -> Hierarchical -> TestCase -> IO (Route, Route, Route)
compareRoute partition raw tileAStar hierarchical testCase = do
  let query = testQuery testCase
      flat = findRoute raw query
      tile = findRoute tileAStar query
      abstract = findRoute hierarchical query
  when (routeCost flat /= routeCost tile || routeCost flat /= routeCost abstract) $ do
    putStrLn "DIFFERENTIAL MISMATCH"
    putStrLn ("case: " <> testLabel testCase)
    putStrLn ("source: " <> coordinateText (queryStart query))
    putStrLn ("target: " <> coordinateText (queryTarget query))
    putStrLn ("query: " <> show query)
    putStrLn ("raw cost: " <> show (routeCost flat))
    putStrLn ("raw route: " <> show (routeSteps flat))
    putStrLn ("tile astar cost: " <> show (routeCost tile))
    putStrLn ("tile astar route: " <> show (routeSteps tile))
    putStrLn ("hierarchical cost: " <> show (routeCost abstract))
    putStrLn ("hierarchical route: " <> show (routeSteps abstract))
    putStrLn ("raw regions: " <> show (regionSequence partition query flat))
    putStrLn ("hierarchical regions: " <> show (regionSequence partition query abstract))
    exitFailure
  pure (flat, tile, abstract)

writeCorpus :: [(TestCase, Route, Route, Route)] -> IO ()
writeCorpus results = do
  createDirectoryIfMissing True "out"
  LBS.writeFile "out/hierarchy-test-routes.json" (encode (map resultJson results))
  putStrLn "wrote out/hierarchy-test-routes.json"
 where
  resultJson (testCase, flat, tile, abstract) =
    object
      [ "name" .= testLabel testCase
      , "source" .= coordinateText (queryStart (testQuery testCase))
      , "target" .= coordinateText (queryTarget (testQuery testCase))
      , "rawCost" .= routeCost flat
      , "tileAStarCost" .= routeCost tile
      , "hierarchicalCost" .= routeCost abstract
      , "path" .= routeStepsJson (routeSteps abstract)
      ]

writeComponentTransformReport :: TileAStar -> IO ()
writeComponentTransformReport (TileAStar _ components _) = do
  createDirectoryIfMissing True "out"
  let rows = componentTransformRows components
      csvPath = "out/component-transform-report.csv"
      mdPath = "out/component-transform-report.md"
  writeFile csvPath (componentTransformCsv rows)
  writeFile mdPath (componentTransformMarkdown rows)
  printf "wrote %s and %s (%d components, %d bbox cells, %d walkable tiles)\n"
    csvPath
    mdPath
    (length rows)
    (sum (map componentTransformArea rows))
    (sum (map componentTransformTiles rows))

writeTileStaticReport :: TileAStar -> IO ()
writeTileStaticReport astar = do
  let (originals, steiners, vertices, edges) = tileStaticStats astar
      cliqueDirected = originals * max 0 (originals - 1)
      bytesEstimate = vertices * 24 + edges * 16
  printf "static original sites: %d\n" originals
  printf "static Steiner vertices: %d\n" steiners
  printf "static total vertices: %d\n" vertices
  printf "sparse walking undirected edges: %d\n" edges
  printf "global complete-clique directed edge proxy: %d\n" cliqueDirected
  printf "rough adjacency memory estimate: %.2f MiB\n" (fromIntegral bytesEstimate / (1024 * 1024) :: Double)

data ComponentTransformRow = ComponentTransformRow
  { componentTransformId :: !Int
  , componentTransformPlane :: !Int
  , componentTransformMinX :: !Int
  , componentTransformMinY :: !Int
  , componentTransformMaxX :: !Int
  , componentTransformMaxY :: !Int
  , componentTransformWidth :: !Int
  , componentTransformHeight :: !Int
  , componentTransformArea :: !Int
  , componentTransformTiles :: !Int
  }

componentTransformRows :: NaturalComponents -> [ComponentTransformRow]
componentTransformRows components =
  map row (filter (not . Vector.null . snd) (Boxed.toList (Boxed.indexed groups)))
 where
  groups = componentTileGroups components
  row (cid, tiles) =
    let box = componentBox tiles
        width = boxMaxX box - boxMinX box + 1
        height = boxMaxY box - boxMinY box + 1
     in ComponentTransformRow
          cid
          (boxPlane box)
          (boxMinX box)
          (boxMinY box)
          (boxMaxX box)
          (boxMaxY box)
          width
          height
          (width * height)
          (Vector.length tiles)

componentTransformCsv :: [ComponentTransformRow] -> String
componentTransformCsv rows =
  unlines
    ( "component,plane,min_x,min_y,max_x,max_y,width,height,bbox_cells,walkable_tiles,fill_ratio"
        : map csvRow rows
    )
 where
  csvRow row =
    show (componentTransformId row)
      <> ","
      <> show (componentTransformPlane row)
      <> ","
      <> show (componentTransformMinX row)
      <> ","
      <> show (componentTransformMinY row)
      <> ","
      <> show (componentTransformMaxX row)
      <> ","
      <> show (componentTransformMaxY row)
      <> ","
      <> show (componentTransformWidth row)
      <> ","
      <> show (componentTransformHeight row)
      <> ","
      <> show (componentTransformArea row)
      <> ","
      <> show (componentTransformTiles row)
      <> ","
      <> printf "%.6f" (fillRatio row)

componentTransformMarkdown :: [ComponentTransformRow] -> String
componentTransformMarkdown rows =
  unlines
    [ "# Component Transform Report"
    , ""
    , "Components: " <> show totalComponents
    , "Walkable tiles: " <> show totalTiles
    , "Bounding-box cells: " <> show totalArea
    , "BBox/walkable multiplier: " <> printf "%.2f" areaMultiplier
    , ""
    , "## Largest Bounding Boxes"
    , ""
    , table (take 30 (sortOn (Down . componentTransformArea) rows))
    , ""
    , "## Sparsest Bounding Boxes"
    , ""
    , table (take 30 (sortOn fillRatio rows))
    ]
 where
  totalComponents = length rows
  totalTiles = sum (map componentTransformTiles rows)
  totalArea = sum (map componentTransformArea rows)
  areaMultiplier = fromIntegral totalArea / fromIntegral (max 1 totalTiles) :: Double
  table selected =
    unlines
      ( "| component | plane | bbox | bbox cells | walkable tiles | fill |"
          : "|---:|---:|---|---:|---:|---:|"
          : map tableRow selected
      )
  tableRow row =
    "| "
      <> show (componentTransformId row)
      <> " | "
      <> show (componentTransformPlane row)
      <> " | "
      <> show (componentTransformMinX row)
      <> ","
      <> show (componentTransformMinY row)
      <> ".."
      <> show (componentTransformMaxX row)
      <> ","
      <> show (componentTransformMaxY row)
      <> " | "
      <> show (componentTransformArea row)
      <> " | "
      <> show (componentTransformTiles row)
      <> " | "
      <> printf "%.4f" (fillRatio row)
      <> " |"

fillRatio :: ComponentTransformRow -> Double
fillRatio row = fromIntegral (componentTransformTiles row) / fromIntegral (max 1 (componentTransformArea row))

routeStepsJson :: [RouteStep] -> [Value]
routeStepsJson = map stepJson
 where
  stepJson (Walk tile) = object ["kind" .= ("walk" :: String), "coordinate" .= coordinateText tile]
  stepJson (UseTransport label tile) = object ["kind" .= ("transport" :: String), "label" .= label, "coordinate" .= coordinateText tile]

serveLoop :: World -> TileAStar -> Maybe Hierarchical -> IO ()
serveLoop world tileAStar hierarchical = do
  LBS.putStrLn (encode (object ["ready" .= True]))
  hFlush stdout
  loop
 where
  loop = do
    done <- isEOF
    when (not done) $ do
      line <- BS.getLine
      response <- serveRequest world tileAStar hierarchical (LBS.fromStrict line)
      LBS.putStrLn (encode response)
      hFlush stdout
      loop

serveRequest :: World -> TileAStar -> Maybe Hierarchical -> LBS.ByteString -> IO Value
serveRequest world tileAStar hierarchical line =
  case eitherDecode line of
    Left message -> pure (object ["ok" .= False, "error" .= ("invalid JSON request: " <> message)])
    Right request
      | not (validPoint (requestStart request)) -> invalid request "start coordinate is outside 0..32767 or has an invalid plane"
      | not (validPoint (requestTarget request)) -> invalid request "target coordinate is outside 0..32767 or has an invalid plane"
      | otherwise -> do
          let query = (defaultQuery (pointTile (requestStart request)) (pointTile (requestTarget request)))
                { allowTransports = requestAllowTransports request
                , heuristicWeight = requestHeuristicWeight request
                }
          case maybe "hierarchical" id (requestFinder request) of
            "raw" -> do
              started <- getMonotonicTimeNSec
              let route = findRoute (RawDijkstra world) query
              _ <- evaluate (routeCost route + routeExpandedNodes route + length (routeSteps route))
              finished <- getMonotonicTimeNSec
              pure (routeResponse request route [] [] [] [] (rawTimingsJson route started finished))
            "tile-full" -> do
              (route, timings, expandedStates) <- findRouteProfiledTileAStarWithTrace (requestIncludeExpandedTiles request) tileAStar query
              pure (routeResponse request route (map fst expandedStates) expandedStates [] [] (tileTimingsJson timings))
            "heuristic" -> do
              let stem = "start-" <> coordinateFileText (queryStart query) <> "-target-" <> coordinateFileText (queryTarget query) <> "-transports-" <> (if allowTransports query then "1" else "0")
                  outputRoot = heuristicTileRoot </> stem
                  urlRoot = "/out/heuristic-tiles/" <> stem
              render <- renderHeuristicTiles tileAStar query outputRoot urlRoot
              pure (heuristicRenderResponse request render)
            "components" -> do
              render <- renderComponentTiles tileAStar componentTileRoot "/out/component-tiles"
              pure (heuristicRenderResponse request render)
            "reverse-path" ->
              pure (reversePathResponse request (reversePathDebug tileAStar query))
            "hierarchical" -> do
              case hierarchical of
                Nothing -> invalid request "hierarchical finder is unavailable in direct mode"
                Just value -> do
                  (route, timings, expandedTiles, heuristicRegions, heuristicTiles) <- findRouteProfiledWithOptions
                    (requestIncludeExpandedTiles request)
                    (requestUseHeuristic request)
                    value
                    query
                  pure (routeResponse request route expandedTiles [] heuristicRegions heuristicTiles (timingsJson timings))
            other -> invalid request ("unknown finder: " <> other)
 where
  invalid :: ServeRequest -> String -> IO Value
  invalid request message = pure (object ["id" .= requestId request, "ok" .= False, "error" .= message])
  pointTile point = packTile (pointX point) (pointY point) (pointPlane point)
  validPoint point = pointX point >= 0 && pointX point <= 32767
    && pointY point >= 0 && pointY point <= 32767
    && pointPlane point >= 0 && pointPlane point <= 3
  coordinateFileText tile = let (x, y, p) = unpackTile tile in show x <> "-" <> show y <> "-" <> show p
  heuristicRegionJson (LeafId component region, value) = object
    [ "component" .= component
    , "region" .= region
    , "value" .= value
    ]
  heuristicTileJson (tile, value) =
    let (x, y, plane) = unpackTile tile
     in object ["x" .= x, "y" .= y, "plane" .= plane, "value" .= value]
  expandedStateJson (tile, banked) =
    let (x, y, plane) = unpackTile tile
     in object ["x" .= x, "y" .= y, "plane" .= plane, "banked" .= banked]
  debugCoordinateJson tile =
    let (x, y, plane) = unpackTile tile
     in object ["x" .= x, "y" .= y, "plane" .= plane]
  debugStateJson state =
    object
      [ "banked" .= reverseStateBanked state
      , "site" .= fmap debugCoordinateJson (reverseStateTile state)
      , "distance" .= reverseStateDistance state
      , "heuristic" .= reverseStateHeuristic state
      , "unreachable" .= reverseStateUnreachable state
      , "path" .= map debugEdgeJson (reverseStatePath state)
      ]
  debugEdgeJson edge =
    object
      [ "from" .= debugCoordinateJson (reverseEdgeFrom edge)
      , "to" .= debugCoordinateJson (reverseEdgeTo edge)
      , "fromBanked" .= reverseEdgeFromBanked edge
      , "toBanked" .= reverseEdgeToBanked edge
      , "type" .= reverseEdgeType edge
      , "label" .= reverseEdgeLabel edge
      , "cost" .= reverseEdgeCost edge
      , "cumulativeCost" .= reverseEdgeCumulativeCost edge
      ]
  routeResponse :: ServeRequest -> Route -> [Tile] -> [(Tile, Bool)] -> [(LeafId, Int)] -> [(Tile, Int)] -> Value -> Value
  routeResponse request route expandedTiles expandedStates heuristicRegions heuristicTiles timings =
    object
      [ "id" .= requestId request
      , "ok" .= True
      , "cost" .= routeCost route
      , "expandedNodes" .= routeExpandedNodes route
      , "path" .= routeStepsJson (routeSteps route)
      , "expandedTiles" .= map coordinateText expandedTiles
      , "expandedStates" .= map expandedStateJson expandedStates
      , "heuristicRegions" .= map heuristicRegionJson heuristicRegions
      , "heuristicTiles" .= map heuristicTileJson heuristicTiles
      , "timings" .= timings
      ]

  heuristicRenderResponse :: ServeRequest -> HeuristicRender -> Value
  heuristicRenderResponse request render =
    object
      [ "id" .= requestId request
      , "ok" .= True
      , "tileSize" .= renderTileSize render
      , "layers" .= map (layerJson (renderTileSize render)) (renderLayers render)
      ]

  reversePathResponse :: ServeRequest -> ReversePathDebug -> Value
  reversePathResponse request debug =
    object
      [ "id" .= requestId request
      , "ok" .= True
      , "seed" .= debugCoordinateJson (reverseDebugSeed debug)
      , "target" .= debugCoordinateJson (reverseDebugTarget debug)
      , "states" .= map debugStateJson (reverseDebugStates debug)
      ]

  layerJson tileSize layer =
    object
      [ "key" .= layerKey layer
      , "label" .= layerLabel layer
      , "bankPathEnabled" .= layerBankPathEnabled layer
      , "min" .= layerMinimum layer
      , "max" .= layerMaximum layer
      , "heuristicMs" .= layerHeuristicMilliseconds layer
      , "transformMs" .= layerTransformMilliseconds layer
      , "writeMs" .= layerWriteMilliseconds layer
      , "seeds" .= map seedJson (layerSeeds layer)
      , "tiles" .= map (tileJson tileSize) (layerTiles layer)
      ]
  seedJson (tile, value) =
    let (x, y, plane) = unpackTile tile
     in object ["x" .= x, "y" .= y, "plane" .= plane, "value" .= value]

  tileJson tileSize tile =
    object
      [ "url" .= tileUrl tile
      , "plane" .= tilePlane tile
      , "x" .= tileX tile
      , "y" .= tileY tile
      , "min" .= tileMinimum tile
      , "max" .= tileMaximum tile
      , "bounds" .=
          [ [tileY tile * tileSize, tileX tile * tileSize]
          , [(tileY tile + 1) * tileSize, (tileX tile + 1) * tileSize]
          ]
      ]

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

rawTimingsJson :: Route -> Word64 -> Word64 -> Value
rawTimingsJson route started finished = object
  [ "setupMs" .= (0 :: Double)
  , "reverseDijkstraMs" .= (0 :: Double)
  , "seedTableMs" .= (0 :: Double)
  , "distanceTransformMs" .= (0 :: Double)
  , "searchMs" .= milliseconds started finished
  , "totalMs" .= milliseconds started finished
  , "search" .= object
      [ "statesPopped" .= routeExpandedNodes route
      , "stalePqEntries" .= (0 :: Int)
      , "pqPushes" .= (0 :: Int)
      , "uniqueStatesReached" .= (0 :: Int)
      , "walkingRelaxations" .= (0 :: Int)
      , "transportRelaxations" .= (0 :: Int)
      , "heuristicEvaluations" .= (0 :: Int)
      ]
  ]

tileTimingsJson :: TileAStarTimings -> Value
tileTimingsJson timings = object
  [ "setupMs" .= tileHeuristicSetupMilliseconds timings
  , "reverseDijkstraMs" .= tileReverseDijkstraMilliseconds timings
  , "seedTableMs" .= tileSeedTableMilliseconds timings
  , "distanceTransformMs" .= (0 :: Double)
  , "searchMs" .= tileSearchMilliseconds timings
  , "totalMs" .= tileTotalMilliseconds timings
  , "search" .= tileCountersJson (tileSearchCounters timings)
  , "reverseSearch" .= tileReverseCountersJson (tileReverseCounters timings)
  ]

tileCountersJson :: TileAStarCounters -> Value
tileCountersJson counters = object
  [ "statesPopped" .= tileStatesPopped counters
  , "stalePqEntries" .= tileStalePqEntries counters
  , "pqPushes" .= tilePqPushes counters
  , "uniqueStatesReached" .= tileUniqueStatesReached counters
  , "walkingRelaxations" .= tileWalkingRelaxations counters
  , "transportRelaxations" .= tileTransportRelaxations counters
  , "heuristicEvaluations" .= tileHeuristicEvaluations counters
  , "heuristicUnreachable" .= tileHeuristicUnreachable counters
  , "unknownComponentPrunes" .= tileUnknownComponentPrunes counters
  , "noReverseSeedPrunes" .= tileNoReverseSeedPrunes counters
  ]

tileReverseCountersJson :: TileReverseCounters -> Value
tileReverseCountersJson counters = object
  [ "statesPopped" .= reverseStatesPopped counters
  , "stalePqEntries" .= reverseStalePqEntries counters
  , "pqPushes" .= reversePqPushes counters
  , "pqPops" .= reversePqPops counters
  , "sameComponentSiteScans" .= reverseSameComponentSiteScans counters
  , "totalSitesScanned" .= reverseTotalSitesScanned counters
  , "chebyshevComparisons" .= reverseChebyshevComparisons counters
  , "maxSitesScannedPerPop" .= reverseMaxSitesScannedPerPop counters
  , "meanSitesScannedPerPop" .= meanSitesScanned counters
  , "transportRelaxations" .= reverseTransportRelaxations counters
  ]
 where
  meanSitesScanned c =
    fromIntegral (reverseTotalSitesScanned c) / fromIntegral (max 1 (reverseSameComponentSiteScans c)) :: Double

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
  , "needBankExpansions" .= searchNeedBankExpansions counters
  , "globalFinishedExpansions" .= searchGlobalFinishedExpansions counters
  , "refinedHeuristicEvaluations" .= searchRefinedHeuristicEvaluations counters
  , "refinedHeuristicCandidates" .= searchRefinedHeuristicCandidates counters
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

loadOrBuildTileAStar :: World -> IO TileAStar
loadOrBuildTileAStar world = do
  fresh <- tileComponentCacheIsFresh
  cached <- if fresh then loadTileComponentCache else pure Nothing
  case cached of
    Just (components, static) -> timedPhase "force cached tile astar components" (forceTileAStar (TileAStar world components static))
    Nothing -> do
      tileAStar@(TileAStar _ components static) <- timedPhase "build tile astar components" (buildTileAStar world)
      createDirectoryIfMissing True "out"
      timedPhase "write tile astar component cache" (encodeFile tileComponentCachePath (TileComponentCache tileComponentCacheVersion components static))
      pure tileAStar

loadTileComponentCache :: IO (Maybe (NaturalComponents, TileStatic))
loadTileComponentCache = timedPhase "load tile astar component cache" $ do
  decoded <- decodeFileOrFail tileComponentCachePath
  case decoded of
    Right (TileComponentCache version components static) | version == tileComponentCacheVersion -> pure (Just (components, static))
    Right _ -> putStrLn "tile astar component cache version mismatch; rebuilding" >> pure Nothing
    Left (_, message) -> putStrLn ("tile astar component cache decode failed; rebuilding: " <> message) >> pure Nothing

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

tileComponentCacheIsFresh :: IO Bool
tileComponentCacheIsFresh = do
  exists <- doesFileExist tileComponentCachePath
  if not exists
    then pure False
    else do
      inputs <- concat <$> mapM filesBelow tileComponentCacheInputRoots
      cacheTime <- getModificationTime tileComponentCachePath
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
regionTableVersion = 2

tileComponentCacheVersion :: Word64
tileComponentCacheVersion = 5

cachePath, partitionPath, regionTablePath, tileComponentCachePath, heuristicTileRoot, componentTileRoot :: FilePath
cachePath = "out/hierarchy-cache.bin"
partitionPath = "out/metis/kahip-partitions.csv"
regionTablePath = "out/region-lower-bounds.bin"
tileComponentCachePath = "out/tile-astar-components.bin"
heuristicTileRoot = "out/heuristic-tiles"
componentTileRoot = "out/component-tiles"

cacheInputRoots :: [FilePath]
cacheInputRoots =
  [ partitionPath
  , resourcesDir defaultSourcePaths
  , "src/ShortestPath/Hierarchy"
  , "src/ShortestPath/Tile.hs"
  , "src/ShortestPath/Transport.hs"
  , "src/ShortestPath/World.hs"
  ]

tileComponentCacheInputRoots :: [FilePath]
tileComponentCacheInputRoots =
  [ resourcesDir defaultSourcePaths
  , "src/ShortestPath/Exact/TileAStar.hs"
  , "src/ShortestPath/Tile.hs"
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
