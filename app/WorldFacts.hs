{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Aeson (FromJSON(..), eitherDecodeFileStrict', withObject, (.:), (.:? ), (.!=))
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.List (intercalate, isInfixOf)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, removeFile, renameFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcess, readProcessWithExitCode)

import ShortestPath.Exact.TileAStar
import ShortestPath.Tile
import ShortestPath.Transport
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
  astar <- buildTileAStar world >>= forceTileAStar
  routes <- loadRoutes "benchmarks/corpus/routes-v1.json"
  wiki <- loadWiki "benchmarks/corpus/wiki-places-v1.json"
  let places = Map.elems (Map.fromList [(placeId p, p) | p <- routePlaces routes <> wikiPlaces wiki])
      points = Set.toAscList (Set.fromList (map placeTile places <> Set.toList (worldBanks world) <> transportPoints world))
      tempDir = output <> ".csv"
      tempDb = output <> ".tmp"
  removeIfExists tempDir
  removeIfExists tempDb
  createDirectoryIfMissing True tempDir
  writeFacts tempDir astar places points
  metadata <- provenance world
  buildDatabase output tempDb tempDir metadata
  removeDirectoryRecursive tempDir
  report astar places
  putStrLn ("wrote " <> output <> " (" <> show (length (tileFacts astar)) <> " tiles, " <> show (length places) <> " places)")

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

writeFacts :: FilePath -> TileAStar -> [Place] -> [Tile] -> IO ()
writeFacts dir astar places points = do
  writeCsv (dir </> "components.csv") ["component_id", "tile_count", "structurally_reachable", "min_x", "max_x", "min_y", "max_y", "min_plane", "max_plane"]
    [[show cid, show count, bool reachable, show loX, show hiX, show loY, show hiY, show loP, show hiP] | (cid, count, reachable, loX, hiX, loY, hiY, loP, hiP) <- componentFacts astar]
  writeCsv (dir </> "tiles.csv") ["x", "y", "plane", "component_id"]
    [[show x, show y, show p, show cid] | (tile, cid) <- tileFacts astar, let (x, y, p) = unpackTile tile]
  writeCsv (dir </> "point_access.csv") ["x", "y", "plane", "resolved_x", "resolved_y", "resolved_plane", "component_id", "access_kind", "structurally_reachable"]
    [ [show x, show y, show p, show rx, show ry, show rp, maybe "" show cid, kind, maybe "" (\value -> bool (IntSet.member value (structurallyReachableIdsOf astar))) cid]
    | point <- points
    , (resolved, cid, kind) <- pointAccessFacts astar point
    , let (x, y, p) = unpackTile point
    , let (rx, ry, rp) = unpackTile resolved
    ]
  writeCsv (dir </> "places.csv") ["place_id", "name", "x", "y", "plane", "source", "source_kind", "metadata"]
    [[placeId place, placeName place, show x, show y, show p, placeSource place, placeKind place, "{}"] | place <- places, let (x, y, p) = unpackTile (placeTile place)]
 where
  bool value = if value then "true" else "false"
  structurallyReachableIdsOf (TileAStar _ components _) = structurallyReachableIds components

writeCsv :: FilePath -> [String] -> [[String]] -> IO ()
writeCsv path headers rows = writeFile path (unlines (map (intercalate "," . map csv) (headers : rows)))

csv :: String -> String
csv value = '"' : concatMap (\c -> if c == '"' then "\"\"" else [c]) value <> "\""

report :: TileAStar -> [Place] -> IO ()
report astar places = do
  let components = componentFacts astar
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
    case pointAccessFacts astar (placeTile place) of
      rows | any (maybe False (\cid -> IntSet.member cid reachable) . second) rows -> "reachable"
      rows | any ((== "unresolved") . thirdAccess) rows -> "unresolved"
      _ -> "unreachable_component"
  reachable = structurallyReachableIdsOf astar
  second (_, value, _) = value
  thirdAccess (_, _, value) = value
  structurallyReachableIdsOf (TileAStar _ components _) = structurallyReachableIds components

provenance :: World -> IO [(String, String)]
provenance world = do
  commit <- readProcess "git" ["rev-parse", "--short", "HEAD"] ""
  dirty <- readProcess "git" ["status", "--porcelain"] ""
  worldHash <- sha256 (collisionZip defaultSourcePaths)
  transportHashes <- mapM (sha256 . (resourcesDir defaultSourcePaths </>) . ("transports" </>) . ttFile) transportTypes
  now <- getCurrentTime
  pure
    [ ("git_commit", trim commit), ("git_dirty", bool (not (null dirty)))
    , ("generated_at", formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
    , ("world_data_hash", worldHash), ("transport_data_hash", intercalate ":" transportHashes)
    , ("component_model_version", "natural-components-v2")
    , ("sailing_supported", "false"), ("generator_version", "world-facts-v1")
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
        <> "CREATE TABLE tiles(x INTEGER, y INTEGER, plane INTEGER, component_id INTEGER);\n"
        <> "CREATE TABLE point_access(x INTEGER, y INTEGER, plane INTEGER, resolved_x INTEGER, resolved_y INTEGER, resolved_plane INTEGER, component_id INTEGER, access_kind VARCHAR, structurally_reachable BOOLEAN);\n"
        <> "CREATE TABLE places(place_id VARCHAR PRIMARY KEY, name VARCHAR, x INTEGER, y INTEGER, plane INTEGER, source VARCHAR, source_kind VARCHAR, metadata JSON);\n"
        <> "INSERT INTO metadata VALUES " <> intercalate "," ["(" <> quote key <> "," <> quote value <> ")" | (key, value) <- metadata] <> ";\n"
        <> copy "components" "components.csv" <> copy "tiles" "tiles.csv" <> copy "point_access" "point_access.csv" <> copy "places" "places.csv"
        <> "CREATE VIEW place_facts AS WITH counts AS (SELECT place_id, count(*) FILTER (WHERE pa.component_id IS NOT NULL) AS attachments, count(DISTINCT pa.component_id) AS components FROM places p LEFT JOIN point_access pa USING (x, y, plane) GROUP BY place_id) SELECT p.place_id, p.name, p.source, p.x, p.y, p.plane, pa.resolved_x, pa.resolved_y, pa.resolved_plane, pa.component_id, pa.access_kind, pa.structurally_reachable, CASE WHEN c.attachments = 0 THEN 'unresolved' WHEN c.components > 1 THEN 'ambiguous/multiple_components' WHEN pa.structurally_reachable THEN 'reachable' ELSE 'unreachable_component' END AS status FROM places p LEFT JOIN point_access pa USING (x, y, plane) JOIN counts c USING (place_id);\n"
  (exitCode, _, errorOutput) <- readProcessWithExitCode "duckdb" [tempDb, "-c", sql] ""
  unless (exitCode == ExitSuccess) (putStrLn errorOutput >> exitFailure)
  removeIfExists output
  renameFile tempDb output

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  isFile <- doesFileExist path
  if isFile then removeFile path else pure ()
