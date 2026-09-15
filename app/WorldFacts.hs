{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Aeson (FromJSON(..), Value, eitherDecodeFileStrict', encode, object, withObject, (.:), (.:? ), (.!=), (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.List (intercalate, isInfixOf, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, removeFile, renameFile)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcess, readProcessWithExitCode)
import Text.Read (readMaybe)

import ShortestPath.Requirements
import ShortestPath.Tile
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.Tsv (field, readRows)
import ShortestPath.World

data Place = Place
  { placeId :: String
  , placeName :: String
  , placeTile :: Tile
  , placeSource :: String
  , placeKind :: String
  }

instance FromJSON Place where
  parseJSON = withObject "place" $ \value -> do
    name <- value .: "name"
    coordinate <- value .: "coordinate"
    source <- value .:? "source" .!= "unknown"
    kind <- value .:? "category" .!= "candidate"
    pure (makePlace name (tile coordinate) source kind)
   where
    tile [x, y, p] = packTile x y p
    tile _ = error "invalid place coordinate"

data WikiPlace = WikiPlace
  { wikiMonster :: String
  , wikiLocation :: String
  , wikiCoordinate :: [Int]
  , wikiSource :: Int
  }

instance FromJSON WikiPlace where
  parseJSON = withObject "wiki place" $ \value -> WikiPlace
    <$> value .: "monster" <*> value .: "location" <*> value .: "coordinate" <*> value .: "source"

newtype WikiDocument = WikiDocument [WikiPlace]

instance FromJSON WikiDocument where
  parseJSON = withObject "wiki document" $ \value -> WikiDocument <$> value .: "places"

data Route = Route
  { routeName :: String
  , routeStart :: [Int]
  , routeTarget :: [Int]
  , routeStartSource :: String
  , routeTargetSource :: String
  , routeCategory :: String
  }

instance FromJSON Route where
  parseJSON = withObject "route" $ \value -> Route
    <$> value .: "name" <*> value .: "start" <*> value .: "target"
    <*> value .: "startSource" <*> value .: "targetSource" <*> value .: "category"

main :: IO ()
main = do
  output <- outputPath <$> getArgs
  createDirectoryIfMissing True (takeDirectory output)
  world <- loadWorld defaultSourcePaths
  topology <- buildWorldTopology world
  routes <- loadRoutes "benchmarks/corpus/routes-v1.json"
  wiki <- loadWiki "benchmarks/corpus/wiki-places-v1.json"
  gpsPath <- fromMaybe "../runelite-gps-plugin/src/main/resources/destinations.tsv" <$> lookupEnv "WORLD_FACTS_GPS_DESTINATIONS"
  gps <- loadGpsPlaces gpsPath
  let places = Map.elems (Map.fromList [(placeId p, p) | p <- routePlaces routes <> wikiPlaces wiki <> gps])
      points = Set.toAscList (Set.fromList (map placeTile places <> Set.toList (worldBanks world) <> transportPoints world))
      tempDir = output <> ".csv"
      tempDb = output <> ".tmp"
  removeIfExists tempDir
  removeIfExists tempDb
  createDirectoryIfMissing True tempDir
  writeFacts tempDir topology places points
  metadata <- (<> topologyMetadata topology) <$> provenance world gpsPath
  buildDatabase output tempDb tempDir metadata
  removeDirectoryRecursive tempDir
  report topology places
  putStrLn ("wrote " <> output <> " (" <> show (length (tileFacts topology)) <> " tiles, " <> show (length places) <> " places)")

outputPath :: [String] -> FilePath
outputPath ["--output", path] = path
outputPath [] = "data/world-facts.duckdb"
outputPath _ = error "usage: world-facts [--output PATH]"

loadRoutes :: FilePath -> IO [Route]
loadRoutes path = either fail pure =<< eitherDecodeFileStrict' path

loadWiki :: FilePath -> IO [WikiPlace]
loadWiki path = do
  WikiDocument value <- either fail pure =<< eitherDecodeFileStrict' path
  pure value

loadGpsPlaces :: FilePath -> IO [Place]
loadGpsPlaces path = do
  exists <- doesFileExist path
  if not exists then pure [] else do
    rows <- zip [2 :: Int ..] <$> readRows path
    pure
      [ makePlace (field "name" row) (packTile x y plane) ("runelite-gps-plugin:" <> path <> ":" <> show line) (field "category" row)
      | (line, row) <- rows
      , Just x <- [readMaybe (field "x" row)]
      , Just y <- [readMaybe (field "y" row)]
      , Just plane <- [readMaybe (field "plane" row)]
      ]

routePlaces :: [Route] -> [Place]
routePlaces = concatMap $ \route ->
  [ makePlace (routeName route <> " (start)") (tile (routeStart route)) (routeStartSource route) (routeCategory route)
  , makePlace (routeName route <> " (target)") (tile (routeTarget route)) (routeTargetSource route) (routeCategory route)
  ]
 where
  tile [x, y, p] = packTile x y p
  tile _ = error "invalid route coordinate"

wikiPlaces :: [WikiPlace] -> [Place]
wikiPlaces = map $ \place -> makePlace (wikiMonster place <> " - " <> wikiLocation place) (tile (wikiCoordinate place)) ("oldschool-wiki:" <> show (wikiSource place)) "monster"
 where
  tile [x, y, p] = packTile x y p
  tile _ = error "invalid wiki coordinate"

makePlace :: String -> Tile -> String -> String -> Place
makePlace name point source kind = Place (source <> "|" <> name <> "|" <> coordinateText point) name point source kind

transportPoints :: World -> [Tile]
transportPoints world = [point | transport <- concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world, point <- maybeToList (origin transport) <> maybeToList (destination transport)]

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]

writeFacts :: FilePath -> WorldTopology -> [Place] -> [Tile] -> IO ()
writeFacts dir topology places points = do
  writeCsv (dir </> "components.csv") ["component_id", "tile_count", "structurally_reachable", "min_x", "max_x", "min_y", "max_y", "min_plane", "max_plane"]
    [[show cid, show count, bool reachable, show loX, show hiX, show loY, show hiY, show loP, show hiP] | (cid, count, reachable, loX, hiX, loY, hiY, loP, hiP) <- componentFacts topology]
  writeCsv (dir </> "tiles.csv") ["x", "y", "plane", "component_id", "routing_component_id"]
    [[show x, show y, show p, show naturalId, show routingId] | (tile, naturalId, routingId) <- routingTileFacts topology, let (x, y, p) = unpackTile tile]
  writeCsv (dir </> "routing_components.csv") ["routing_component_id", "natural_component_id", "tile_count", "separator_crossing_count", "neighbouring_component_count"]
    [[show routingId, show naturalId, show count, show crossings, show neighbours] | (routingId, naturalId, count, crossings, neighbours) <- routingComponentFacts topology]
  writeCsv (dir </> "separator_crossings.csv") ["from_routing_component", "to_routing_component", "from_x", "from_y", "from_plane", "to_x", "to_y", "to_plane", "cost"]
    [ [show (crossingFromComponent edge), show (crossingToComponent edge), show fx, show fy, show fp, show tx, show ty, show tp, show (crossingCost edge)]
    | edge <- separatorCrossingFacts topology
    , let (fx, fy, fp) = unpackTile (crossingFromTile edge)
    , let (tx, ty, tp) = unpackTile (crossingToTile edge)
    ]
  writeCsv (dir </> "point_access.csv") ["x", "y", "plane", "resolved_x", "resolved_y", "resolved_plane", "component_id", "access_kind", "structurally_reachable"]
    [ [show x, show y, show p, show rx, show ry, show rp, maybe "" show cid, kind, maybe "" (\value -> bool (IntSet.member value (structurallyReachableIdsOf topology))) cid]
    | point <- points
    , (resolved, cid, kind) <- pointAccessFacts topology point
    , let (x, y, p) = unpackTile point
    , let (rx, ry, rp) = unpackTile resolved
    ]
  writeCsv (dir </> "places.csv") ["place_id", "name", "x", "y", "plane", "source", "source_kind", "metadata"]
    [[placeId place, placeName place, show x, show y, show p, placeSource place, placeKind place, "{}"] | place <- places, let (x, y, p) = unpackTile (placeTile place)]
  writeCsv (dir </> "transports.csv")
    [ "transport_id", "transport_type"
    , "origin_x", "origin_y", "origin_plane"
    , "destination_x", "destination_y", "destination_plane"
    , "duration", "display_info", "object_info", "consumable", "max_wilderness_level", "source", "requirements_json"
    ]
    [ transportRow transport
    | transport <- allWorldTransports (topologyWorld topology)
    ]
 where
  bool value = if value then "true" else "false"
  structurallyReachableIdsOf = structurallyReachableIds . topologyStructuralReachability

topologyMetadata :: WorldTopology -> [(String, String)]
topologyMetadata topology =
  [ ("natural_component_count", show (length natural))
  , ("routing_component_count", show (length routing))
  , ("natural_components_split", show splitNatural)
  , ("routing_component_size_p50", show (percentile 50 sizes))
  , ("routing_component_size_p90", show (percentile 90 sizes))
  , ("routing_component_size_p95", show (percentile 95 sizes))
  , ("routing_component_size_p99", show (percentile 99 sizes))
  , ("routing_component_size_max", show (percentile 100 sizes))
  , ("separator_crossing_count", show (length crossings))
  , ("separator_boundary_tile_count", show boundaryTiles)
  , ("separator_crossings_per_component_p50", show (percentile 50 crossingCounts))
  , ("separator_crossings_per_component_p95", show (percentile 95 crossingCounts))
  , ("separator_crossings_per_component_p99", show (percentile 99 crossingCounts))
  , ("separator_crossings_per_component_max", show (percentile 100 crossingCounts))
  ]
 where
  natural = componentFacts topology
  routing = routingComponentFacts topology
  crossings = separatorCrossingFacts topology
  sizes = sort [count | (_, _, count, _, _) <- routing]
  crossingCounts = sort [count | (_, _, _, count, _) <- routing]
  splitNatural = length (filter (> 1) (Map.elems (Map.fromListWith (+) [(naturalId, 1 :: Int) | (_, naturalId, _, _, _) <- routing])))
  boundaryTiles = Set.size (Set.fromList [tile | edge <- crossings, tile <- [crossingFromTile edge, crossingToTile edge]])
  percentile _ [] = 0
  percentile p xs = xs !! ((p * length xs + 99) `div` 100 - 1)

allWorldTransports :: World -> [Transport]
allWorldTransports world = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world

transportRow :: Transport -> [String]
transportRow transport =
  [ transportId transport
  , transportType transport
  ] <> tileFields (origin transport)
    <> tileFields (destination transport)
    <> [ show (duration transport)
       , displayInfo transport
       , objectInfo transport
       , bool (consumable transport)
       , maybe "" show (maxWildernessLevel transport)
       , source transport
       , requirementsText transport
       ]
 where
  bool value = if value then "true" else "false"

transportId :: Transport -> String
transportId transport = intercalate "|"
  [ transportType transport
  , maybe "" coordinateText (origin transport)
  , maybe "" coordinateText (destination transport)
  , show (duration transport)
  , displayInfo transport
  , objectInfo transport
  , show (consumable transport)
  , maybe "" show (maxWildernessLevel transport)
  , source transport
  , requirementsText transport
  ]

tileFields :: Maybe Tile -> [String]
tileFields Nothing = ["", "", ""]
tileFields (Just tile) = let (x, y, p) = unpackTile tile in [show x, show y, show p]

requirementsText :: Transport -> String
requirementsText = LBS.unpack . encode . requirementsJson

requirementsJson :: Transport -> Value
requirementsJson transport = object
  [ "skills" .= [object ["level" .= skillLevel req, "skill" .= skillName req] | req <- skills transport]
  , "items" .= fmap itemExprJson (items transport)
  , "quests" .= quests transport
  , "varbits" .= [varReqJson req | req <- varbits transport]
  , "varplayers" .= [varReqJson req | req <- varPlayers transport]
  ]

itemExprJson :: ItemExpr -> Value
itemExprJson (ItemOne term) = object ["kind" .= ("item" :: String), "name" .= itemName term, "ids" .= itemIds term, "quantity" .= itemQuantity term]
itemExprJson (ItemAnd terms) = object ["kind" .= ("and" :: String), "terms" .= map itemExprJson terms]
itemExprJson (ItemOr terms) = object ["kind" .= ("or" :: String), "terms" .= map itemExprJson terms]

varReqJson :: VarReq -> Value
varReqJson req = object
  [ "id" .= varId (varRef req)
  , "op" .= varOpText (varOp req)
  , "value" .= varValue req
  ]

varId :: GameVar -> Int
varId (GameVarbit (VarbitId value)) = value
varId (GameVarPlayer (VarPlayerId value)) = value

varOpText :: VarOp -> String
varOpText VarEq = "="
varOpText VarGt = ">"
varOpText VarLt = "<"
varOpText VarMask = "&"
varOpText VarCooldownMinutes = "@"

writeCsv :: FilePath -> [String] -> [[String]] -> IO ()
writeCsv path headers rows = writeFile path (unlines (map (intercalate "," . map csv) (headers : rows)))

csv :: String -> String
csv value = '"' : concatMap (\c -> if c == '"' then "\"\"" else [c]) value <> "\""

report :: WorldTopology -> [Place] -> IO ()
report topology places = do
  let components = componentFacts topology
      statuses = map placeStatus places
      count :: String -> Int
      count status = length (filter (== status) statuses)
      water = filter (\p -> any (`contains` lower (placeName p)) ["sea", "ocean", "strait", "bay", "atoll", "coast", "passage"]) places
  putStrLn ("components: " <> show (length components) <> " (" <> show (length (filter third components)) <> " structurally reachable)")
  putStrLn ("places: " <> show (length places) <> ", reachable=" <> show (count "reachable") <> ", unreachable=" <> show (count "unreachable_component") <> ", unresolved=" <> show (count "unresolved") <> ", water-like=" <> show (length water))
 where
  third (_, _, value, _, _, _, _, _, _) = value
  lower = map toLowerAscii
  toLowerAscii c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
  contains needle haystack = needle `isInfixOf` haystack
  placeStatus place =
    case pointAccessFacts topology (placeTile place) of
      rows | any (maybe False (\cid -> IntSet.member cid reachable) . second) rows -> "reachable"
      rows | any ((== "unresolved") . thirdAccess) rows -> "unresolved"
      _ -> "unreachable_component"
  reachable = structurallyReachableIds (topologyStructuralReachability topology)
  second (_, value, _) = value
  thirdAccess (_, _, value) = value

pointAccessFacts :: WorldTopology -> Tile -> [(Tile, Maybe Int, String)]
pointAccessFacts topology point =
  [(resolved, Just cid, accessKind) | (resolved, cid) <- attachments]
    <> [(point, Nothing, "unresolved") | null attachments]
 where
  attachments = pointAttachmentDetails topology point
  world = topologyWorld topology
  accessKind
    | isWalkable (worldCollision world) point = "walkable"
    | Map.member point (worldTransports world) = "adjacent_transport_origin"
    | otherwise = "snapped"

provenance :: World -> FilePath -> IO [(String, String)]
provenance world gpsPath = do
  commit <- readProcess "git" ["rev-parse", "--short", "HEAD"] ""
  dirty <- readProcess "git" ["status", "--porcelain"] ""
  worldHash <- sha256 (collisionZip defaultSourcePaths)
  transportHashes <- mapM (sha256 . (resourcesDir defaultSourcePaths </>) . ("transports" </>) . ttFile) transportTypes
  gpsExists <- doesFileExist gpsPath
  gpsHash <- if gpsExists then sha256 gpsPath else pure "missing"
  now <- getCurrentTime
  pure
    [ ("git_commit", trim commit), ("git_dirty", bool (not (null dirty)))
    , ("generated_at", formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
    , ("world_data_hash", worldHash), ("transport_data_hash", intercalate ":" transportHashes)
    , ("place_data_hash", gpsHash)
    , ("component_model_version", "world-topology-v4")
    , ("sailing_supported", "false"), ("generator_version", "world-facts-v4")
    , ("walkable_tiles", show (length (collisionTiles (worldCollision world))))
    ]
 where
  trim = takeWhile (/= '\n')
  bool value = if value then "true" else "false"

sha256 :: FilePath -> IO String
sha256 path = takeWhile (/= ' ') <$> readProcess "sha256sum" [path] ""

buildDatabase :: FilePath -> FilePath -> FilePath -> [(String, String)] -> IO ()
buildDatabase output tempDb dir metadata = do
  let quote value = "'" <> concatMap (\c -> if c == '\'' then "''" else [c]) value <> "'"
      copy table file = "COPY " <> table <> " FROM " <> quote (dir </> file) <> " (HEADER, DELIM ',', QUOTE '\"', ESCAPE '\"', NULL '');\n"
      sql = "CREATE TABLE metadata(key VARCHAR PRIMARY KEY, value VARCHAR);\n"
        <> "CREATE TABLE components(component_id INTEGER PRIMARY KEY, tile_count INTEGER, structurally_reachable BOOLEAN, min_x INTEGER, max_x INTEGER, min_y INTEGER, max_y INTEGER, min_plane INTEGER, max_plane INTEGER);\n"
        <> "CREATE TABLE tiles(x INTEGER, y INTEGER, plane INTEGER, component_id INTEGER, routing_component_id INTEGER);\n"
        <> "CREATE TABLE routing_components(routing_component_id INTEGER PRIMARY KEY, natural_component_id INTEGER, tile_count INTEGER, separator_crossing_count INTEGER, neighbouring_component_count INTEGER);\n"
        <> "CREATE TABLE separator_crossings(from_routing_component INTEGER, to_routing_component INTEGER, from_x INTEGER, from_y INTEGER, from_plane INTEGER, to_x INTEGER, to_y INTEGER, to_plane INTEGER, cost INTEGER);\n"
        <> "CREATE TABLE point_access(x INTEGER, y INTEGER, plane INTEGER, resolved_x INTEGER, resolved_y INTEGER, resolved_plane INTEGER, component_id INTEGER, access_kind VARCHAR, structurally_reachable BOOLEAN);\n"
        <> "CREATE TABLE places(place_id VARCHAR PRIMARY KEY, name VARCHAR, x INTEGER, y INTEGER, plane INTEGER, source VARCHAR, source_kind VARCHAR, metadata JSON);\n"
        <> "CREATE TABLE transports(transport_id VARCHAR PRIMARY KEY, transport_type VARCHAR, origin_x INTEGER, origin_y INTEGER, origin_plane INTEGER, destination_x INTEGER, destination_y INTEGER, destination_plane INTEGER, duration INTEGER, display_info VARCHAR, object_info VARCHAR, consumable BOOLEAN, max_wilderness_level INTEGER, source VARCHAR, requirements_json JSON);\n"
        <> "INSERT INTO metadata VALUES " <> intercalate "," ["(" <> quote key <> "," <> quote value <> ")" | (key, value) <- metadata] <> ";\n"
        <> copy "components" "components.csv" <> copy "tiles" "tiles.csv" <> copy "routing_components" "routing_components.csv" <> copy "separator_crossings" "separator_crossings.csv" <> copy "point_access" "point_access.csv" <> copy "places" "places.csv" <> copy "transports" "transports.csv"
        <> "CREATE VIEW reachable_tiles AS SELECT t.* FROM tiles t JOIN components c USING (component_id) WHERE c.structurally_reachable;\n"
        <> "CREATE VIEW reachable_point_access AS SELECT pa.* FROM point_access pa WHERE pa.structurally_reachable;\n"
        <> "CREATE VIEW place_facts AS WITH counts AS (SELECT place_id, count(*) FILTER (WHERE pa.component_id IS NOT NULL) AS attachments, count(DISTINCT pa.component_id) AS components FROM places p LEFT JOIN point_access pa USING (x, y, plane) GROUP BY place_id) SELECT p.place_id, p.name, p.source, p.x, p.y, p.plane, pa.resolved_x, pa.resolved_y, pa.resolved_plane, pa.component_id, pa.access_kind, pa.structurally_reachable, CASE WHEN c.attachments = 0 THEN 'unresolved' WHEN c.components > 1 THEN 'ambiguous/multiple_components' WHEN pa.structurally_reachable THEN 'reachable' ELSE 'unreachable_component' END AS status FROM places p LEFT JOIN point_access pa USING (x, y, plane) JOIN counts c USING (place_id);\n"
        <> "CREATE VIEW transport_facts AS SELECT t.*, oa.resolved_x AS origin_resolved_x, oa.resolved_y AS origin_resolved_y, oa.resolved_plane AS origin_resolved_plane, oa.component_id AS origin_component_id, oa.access_kind AS origin_access_kind, oa.structurally_reachable AS origin_structurally_reachable, da.resolved_x AS destination_resolved_x, da.resolved_y AS destination_resolved_y, da.resolved_plane AS destination_resolved_plane, da.component_id AS destination_component_id, da.access_kind AS destination_access_kind, da.structurally_reachable AS destination_structurally_reachable FROM transports t LEFT JOIN point_access oa ON t.origin_x = oa.x AND t.origin_y = oa.y AND t.origin_plane = oa.plane LEFT JOIN point_access da ON t.destination_x = da.x AND t.destination_y = da.y AND t.destination_plane = da.plane;\n"
  (exitCode, _, errorOutput) <- readProcessWithExitCode "duckdb" [tempDb, "-c", sql] ""
  unless (exitCode == ExitSuccess) (putStrLn errorOutput >> exitFailure)
  removeIfExists output
  renameFile tempDb output

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  isFile <- doesFileExist path
  if isFile then removeFile path else pure ()
