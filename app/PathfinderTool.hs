{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (filterM, when)
import Data.Aeson (FromJSON(..), Value, encode, eitherDecode, object, withObject, (.:), (.:?), (.=), (.!=))
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Ord (Down(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as BS
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

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.Configuration
import ShortestPath.Exact.TileAStar.Debug
import ShortestPath.Exact.TileAStar.Preprocessing (componentTileGroups)
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.DistanceTransform
import ShortestPath.Exact.ReferenceDijkstra (ReferenceDijkstra(..), findRouteReferenceDijkstra)
import ShortestPath.Account
  ( AccountState(..), CooldownState(..), PohBuild(..), PohPortalAccess(..), RequirementMode(..), RuntimeState(..) )
import ShortestPath.BenchmarkProfiles (benchmarkAccount, benchmarkProfileNames, benchmarkNowMinutes)
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

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
  , requestHeuristicWeight :: Double
  , requestFinder :: Maybe String
  , requestAccountProfile :: Maybe String
  }

instance FromJSON ServeRequest where
  parseJSON = withObject "route request" $ \value ->
    ServeRequest
      <$> value .: "id"
      <*> value .: "start"
      <*> value .: "target"
      <*> value .: "allowTransports"
      <*> value .: "includeExpandedTiles"
      <*> value .:? "heuristicWeight" .!= 1
      <*> value .:? "finder"
      <*> value .:? "accountProfile"

data TileComponentCache = TileComponentCache Word64 NaturalComponents TileStatic
  deriving stock (Generic)
  deriving anyclass (Binary)

data RoutingPreparationCache = RoutingPreparationCache
  (Maybe (RoutingOptions, CompiledRoutingAccount))
  (Maybe (EffectiveRoutingFingerprint, Tile, PreparedTarget))

main :: IO ()
main = do
  command <- parseCommand =<< getArgs
  world <- timedPhase "load world" (loadWorld defaultSourcePaths)
  tileAStar <- loadOrBuildTileAStar world
  tileConfig <- tileAStarConfigFromEnvironment
  useCTransform <- tileUseCTransformFromEnvironment
  case command of
    Serve -> serveLoop tileConfig useCTransform world tileAStar
    ComponentTransformReport -> writeComponentTransformReport tileAStar
    TileStaticReport -> writeTileStaticReport tileAStar

data Command = Serve | ComponentTransformReport | TileStaticReport
  deriving (Eq, Show)

parseCommand :: [String] -> IO Command
parseCommand ["serve"] = pure Serve
parseCommand ["component-transform-report"] = pure ComponentTransformReport
parseCommand ["tile-static-report"] = pure TileStaticReport
parseCommand _ = do
  putStrLn "usage: pathfinder-tool serve|component-transform-report|tile-static-report"
  exitFailure

writeComponentTransformReport :: TileAStar -> IO ()
writeComponentTransformReport astar = do
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
 where
  components = topologyNaturalComponents (tileTopology astar)

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

serveLoop :: TileAStarConfig -> Bool -> World -> TileAStar -> IO ()
serveLoop tileConfig useCTransform world tileAStar = do
  cache <- newIORef (RoutingPreparationCache Nothing Nothing)
  LBS.putStrLn (encode (object ["ready" .= True]))
  hFlush stdout
  loop cache
 where
  loop cache = do
    done <- isEOF
    when (not done) $ do
      line <- BS.getLine
      response <- serveRequest cache tileConfig useCTransform world tileAStar (LBS.fromStrict line)
      LBS.putStrLn (encode response)
      hFlush stdout
      loop cache

serveRequest :: IORef RoutingPreparationCache -> TileAStarConfig -> Bool -> World -> TileAStar -> LBS.ByteString -> IO Value
serveRequest cache tileConfig useCTransform world tileAStar line =
  case eitherDecode line of
    Left message -> pure (object ["ok" .= False, "error" .= ("invalid JSON request: " <> message)])
    Right request
      | not (validPoint (requestStart request)) -> invalid request "start coordinate is outside 0..32767 or has an invalid plane"
      | not (validPoint (requestTarget request)) -> invalid request "target coordinate is outside 0..32767 or has an invalid plane"
      | maybe False (`notElem` profileNames) (requestAccountProfile request) -> invalid request "unknown account profile"
      | otherwise -> do
          let profile = requestAccountProfile request >>= \name -> benchmarkAccount name (allTransports world)
          let query = (defaultQuery (pointTile (requestStart request)) (pointTile (requestTarget request)))
                { allowTransports = requestAllowTransports request
                , heuristicWeight = requestHeuristicWeight request
                , requirementMode = maybe IgnoreRequirements ConfiguredRequirements profile
                , queryNowMinutes = benchmarkNowMinutes
                }
          case maybe "tile-full" id (requestFinder request) of
            "reference" -> do
              started <- getMonotonicTimeNSec
              let route = findRouteReferenceDijkstra (ReferenceDijkstra (tileTopology tileAStar)) query
              _ <- evaluate (routeCost route + routeExpandedNodes route + length (routeSteps route))
              finished <- getMonotonicTimeNSec
              pure (routeResponse request route [] [] (rawTimingsJson route started finished))
            "tile-full" -> do
              (account, accountMs, target, targetMs) <- prepareCached cache tileConfig tileAStar query
              (route, searchTimings, expandedStates) <- searchPreparedProfiledWithTrace
                (requestIncludeExpandedTiles request) tileAStar account target (queryStart query) (searchOptionsFromQuery query)
              let timings = recordPreparationTimings accountMs targetMs target searchTimings
              pure (routeResponse request route (map fst expandedStates) expandedStates (tileTimingsJson timings))
            "heuristic" -> do
              let stem = "start-" <> coordinateFileText (queryStart query) <> "-target-" <> coordinateFileText (queryTarget query) <> "-transports-" <> (if allowTransports query then "1" else "0")
                  outputRoot = heuristicTileRoot </> stem
                  urlRoot = "/out/heuristic-tiles/" <> stem
              render <- renderHeuristicTilesWithTransform useCTransform tileAStar query outputRoot urlRoot
              pure (heuristicRenderResponse request render)
            "components" -> do
              render <- renderComponentTiles tileAStar componentTileRoot "/out/component-tiles"
              pure (heuristicRenderResponse request render)
            "profiles" ->
              pure (object
                [ "id" .= requestId request
                , "ok" .= True
                , "profiles" .=
                    [ accountJson name account
                    | name <- benchmarkProfileNames
                    , Just account <- [benchmarkAccount name (allTransports world)]
                    ]
                ])
            "reverse-path" ->
              pure (reversePathResponse request (reversePathDebug tileAStar query))
            other -> invalid request ("unknown finder: " <> other)
 where
  invalid :: ServeRequest -> String -> IO Value
  invalid request message = pure (object ["id" .= requestId request, "ok" .= False, "error" .= message])
  pointTile point = packTile (pointX point) (pointY point) (pointPlane point)
  validPoint point = pointX point >= 0 && pointX point <= 32767
    && pointY point >= 0 && pointY point <= 32767
    && pointPlane point >= 0 && pointPlane point <= 3
  coordinateFileText tile = let (x, y, p) = unpackTile tile in show x <> "-" <> show y <> "-" <> show p
  profileNames = benchmarkProfileNames
  allTransports value = concat (Map.elems (worldTransports value)) <> worldGlobalTeleports value
  accountJson name account = object
    [ "name" .= name
    , "levels" .= accountLevels account
    , "questCount" .= Set.size (accountCompletedQuests account)
    , "inventory" .= accountInventory account
    , "equipment" .= accountEquipment account
    , "runePouch" .= accountRunePouch account
    , "bank" .= accountBank account
    , "diaries" .= Map.fromList [(show diary, show tier) | (diary, tier) <- Map.toList (accountDiaries account)]
    , "fairyRings" .= accountFairyRingsUnlocked account
    , "poh" .= pohJson (accountPoh account)
    , "runtime" .= runtimeJson (accountRuntime account)
    ]

  pohJson poh = object
    [ "location" .= show (pohLocation poh)
    , "jewelleryBox" .= show (pohJewelleryBox poh)
    , "portals" .= case pohPortalDestinations poh of
        AllPohPortals -> ["*"]
        SelectedPohPortals destinations -> Set.toAscList destinations
    , "fairyRing" .= pohFairyRing poh
    , "spiritTree" .= pohSpiritTree poh
    , "obelisk" .= pohObelisk poh
    , "mountedGlory" .= pohMountedGlory poh
    , "mountedXerics" .= pohMountedXerics poh
    , "mountedDigsite" .= pohMountedDigsite poh
    , "mountedMythical" .= pohMountedMythical poh
    ]
  runtimeJson runtime = object
    [ "spellbook" .= show (runtimeSpellbook runtime)
    , "cooldownsReady" .= (runtimeMinigameTeleport runtime == CooldownReady)
    , "arriveInsidePoh" .= runtimeArriveInsidePoh runtime
    ]
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
  routeResponse :: ServeRequest -> Route -> [Tile] -> [(Tile, Bool)] -> Value -> Value
  routeResponse request route expandedTiles expandedStates timings =
    object
      [ "id" .= requestId request
      , "ok" .= True
      , "cost" .= routeCost route
      , "expandedNodes" .= routeExpandedNodes route
      , "path" .= routeStepsJson (routeSteps route)
      , "expandedTiles" .= map coordinateText expandedTiles
      , "expandedStates" .= map expandedStateJson expandedStates
      , "heuristicRegions" .= ([] :: [Value])
      , "heuristicTiles" .= ([] :: [Value])
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

prepareCached :: IORef RoutingPreparationCache -> TileAStarConfig -> TileAStar -> Query
  -> IO (CompiledRoutingAccount, Double, PreparedTarget, Double)
prepareCached ref config astar query = do
  RoutingPreparationCache cachedAccount cachedTarget <- readIORef ref
  let options = routingOptionsFromQuery query
  (account, accountMs) <- case cachedAccount of
    Just (cachedOptions, cached) | cachedOptions == options -> pure (cached, 0)
    _ -> do
      (candidate, elapsed) <- compileRoutingAccountProfiled astar options
      let selected = case cachedAccount of
            Just (_, cached) | compiledRoutingFingerprint cached == compiledRoutingFingerprint candidate -> cached
            _ -> candidate
      pure (selected, elapsed)
  let fingerprint = compiledRoutingFingerprint account
      destination = queryTarget query
  (target, targetMs) <- case cachedTarget of
    Just (cachedFingerprint, cachedDestination, cached)
      | cachedFingerprint == fingerprint && cachedDestination == destination -> pure (cached, 0)
    _ -> prepareTargetProfiled config astar account destination
  writeIORef ref (RoutingPreparationCache (Just (options, account)) (Just (fingerprint, destination, target)))
  pure (account, accountMs, target, targetMs)

rawTimingsJson :: Route -> Word64 -> Word64 -> Value
rawTimingsJson route started finished = object
  [ "accountPrepareMs" .= (0 :: Double)
  , "targetPrepareMs" .= (0 :: Double)
  , "forwardSearchMs" .= milliseconds started finished
  , "setupMs" .= (0 :: Double)
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
      , "heuristicCalls" .= (0 :: Int)
      , "heuristicCandidatesScanned" .= (0 :: Int)
      , "heuristicMaxCandidatesPerCall" .= (0 :: Int)
      ]
  ]

tileTimingsJson :: TileAStarTimings -> Value
tileTimingsJson timings = object
  [ "mode" .= timingMode timings
  , "accountPrepareMs" .= tileAccountPrepareMilliseconds timings
  , "targetPrepareMs" .= tileTargetPrepareMilliseconds timings
  , "forwardSearchMs" .= tileSearchMilliseconds timings
  , "setupMs" .= tileHeuristicSetupMilliseconds timings
  , "reverseDijkstraMs" .= tileReverseDijkstraMilliseconds timings
  , "seedTableMs" .= tileSeedTableMilliseconds timings
  , "heuristicSeedCount" .= tileHeuristicSeedCount timings
  , "heuristicComponentCount" .= tileHeuristicComponentCount timings
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
  , "distanceTransformMs" .= (0 :: Double)
  , "searchMs" .= tileSearchMilliseconds timings
  , "totalMs" .= tileTotalMilliseconds timings
  , "search" .= tileCountersJson (tileSearchCounters timings)
  , "reverseSearch" .= tileReverseCountersJson (tileReverseCounters timings)
  ]

timingMode :: TileAStarTimings -> String
timingMode timings
  | tileTargetPrepareMilliseconds timings == 0 = "warm-target"
  | tileAccountPrepareMilliseconds timings == 0 = "warm-account"
  | otherwise = "cold"

tileCountersJson :: TileAStarCounters -> Value
tileCountersJson counters = object
  [ "statesPopped" .= tileStatesPopped counters
  , "stalePqEntries" .= tileStalePqEntries counters
  , "pqPushes" .= tilePqPushes counters
  , "uniqueStatesReached" .= tileUniqueStatesReached counters
  , "walkingRelaxations" .= tileWalkingRelaxations counters
  , "transportRelaxations" .= tileTransportRelaxations counters
  , "heuristicEvaluations" .= tileHeuristicEvaluations counters
  , "heuristicCalls" .= tileHeuristicCalls counters
  , "heuristicCandidatesScanned" .= tileHeuristicCandidatesScanned counters
  , "heuristicMaxCandidatesPerCall" .= tileHeuristicMaxCandidatesPerCall counters
  , "heuristicUnreachable" .= tileHeuristicUnreachable counters
  , "unknownComponentPrunes" .= tileUnknownComponentPrunes counters
  , "noReverseSeedPrunes" .= tileNoReverseSeedPrunes counters
  , "bestBankCostUpdates" .= tileBestBankCostUpdates counters
  , "finalBestBankCost" .= tileFinalBestBankCost counters
  , "bankDominatedHeuristicEvaluations" .= tileBankDominatedHeuristicEvaluations counters
  , "bankGlobalTransitionsSuppressed" .= tileBankGlobalTransitionsSuppressed counters
  , "bankBoundPQRekeys" .= tileBankBoundPQRekeys counters
  , "bankGlobalTrace" .= map bankGlobalTraceJson (tileBankGlobalTrace counters)
  ]

bankGlobalTraceJson :: TileBankGlobalObservation -> Value
bankGlobalTraceJson observation = object
  [ "tile" .= traceTileJson (bankGlobalTile observation)
  , "stateBanked" .= bankGlobalStateBanked observation
  , "cost" .= bankGlobalCost observation
  , "bestBankCost" .= bankGlobalBestBankCost observation
  , "suppressed" .= bankGlobalSuppressed observation
  , "edges" .= map edgeJson (bankGlobalEdges observation)
  ]
 where
  edgeJson (destination, cost, label) = object ["destination" .= traceTileJson destination, "cost" .= cost, "label" .= label]

traceTileJson :: Tile -> [Int]
traceTileJson tile = let (x, y, plane) = unpackTile tile in [x, y, plane]

tileReverseCountersJson :: TileReverseCounters -> Value
tileReverseCountersJson counters = object
  [ "statesPopped" .= reverseStatesPopped counters
  , "stalePqEntries" .= reverseStalePqEntries counters
  , "pqPushes" .= reversePqPushes counters
  , "pqPops" .= reversePqPops counters
  , "pqMaxSize" .= reversePqMaxSize counters
  , "edgesRelaxed" .= reverseEdgesRelaxed counters
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

timedPhase :: String -> IO a -> IO a
timedPhase label action = do
  putStrLn ("pathfinder tool: " <> label)
  hFlush stdout
  started <- getMonotonicTimeNSec
  result <- action
  finished <- getMonotonicTimeNSec
  printf "pathfinder tool: %s completed in %.1f ms\n" label (milliseconds started finished)
  hFlush stdout
  pure result

milliseconds :: Word64 -> Word64 -> Double
milliseconds started finished = fromIntegral (finished - started) / 1000000

loadOrBuildTileAStar :: World -> IO TileAStar
loadOrBuildTileAStar world = do
  fresh <- tileComponentCacheIsFresh
  cached <- if fresh then loadTileComponentCache else pure Nothing
  case cached of
    Just (components, static) ->
      case worldTopologyFromComponents productionStructuralReachabilityPolicy world components of
        Left err -> fail (show err)
        Right topology -> timedPhase "force cached tile astar components" (forceTileAStar (TileAStar topology static))
    Nothing -> do
      tileAStar@(TileAStar topology static) <- timedPhase "build tile astar components" (buildTileAStar world)
      let components = topologyNaturalComponents topology
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

tileComponentCacheIsFresh :: IO Bool
tileComponentCacheIsFresh = do
  exists <- doesFileExist tileComponentCachePath
  if not exists
    then pure False
    else do
      inputs <- concat <$> mapM filesBelow tileComponentCacheInputRoots
      cacheTime <- getModificationTime tileComponentCachePath
      and <$> mapM (fmap (<= cacheTime) . getModificationTime) inputs

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

tileComponentCacheVersion :: Word64
tileComponentCacheVersion = 12

tileComponentCachePath, heuristicTileRoot, componentTileRoot :: FilePath
tileComponentCachePath = "out/tile-astar-components.bin"
heuristicTileRoot = "out/heuristic-tiles"
componentTileRoot = "out/component-tiles"

tileComponentCacheInputRoots :: [FilePath]
tileComponentCacheInputRoots =
  [ resourcesDir defaultSourcePaths
  , "src/ShortestPath/Exact/TileAStar.hs"
  , "src/ShortestPath/Exact/TileAStar"
  , "src/ShortestPath/Topology.hs"
  , "src/ShortestPath/Tile.hs"
  , "src/ShortestPath/World.hs"
  ]
