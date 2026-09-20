{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (filterM)
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Ord (Down(..))
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Vector
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import Data.List (sortOn)
import Text.Printf (printf)

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.Preprocessing (componentTileGroups)
import ShortestPath.Exact.TileAStar.StaticArtifact
import ShortestPath.Exact.TileAStar.Types
import ShortestPath.Internal.DistanceTransform
import ShortestPath.Topology
import ShortestPath.Transport
import ShortestPath.World

data TileComponentCache = TileComponentCache Word64 NaturalComponents TileStatic
  deriving stock (Generic)
  deriving anyclass (Binary)

main :: IO ()
main = do
  command <- parseCommand =<< getArgs
  world <- timedPhase "load world" (loadWorld defaultSourcePaths)
  tileAStar <- loadOrBuildTileAStar world
  case command of
    ComponentTransformReport -> writeComponentTransformReport tileAStar
    TileStaticReport -> writeTileStaticReport tileAStar
    ExportRoutingStatic path -> writeRoutingStaticReport path tileAStar

data Command = ComponentTransformReport | TileStaticReport | ExportRoutingStatic FilePath
  deriving (Eq, Show)

parseCommand :: [String] -> IO Command
parseCommand ["component-transform-report"] = pure ComponentTransformReport
parseCommand ["tile-static-report"] = pure TileStaticReport
parseCommand ["export-routing-static"] = pure (ExportRoutingStatic "out/routing-static-v1.bin")
parseCommand ["export-routing-static", path] = pure (ExportRoutingStatic path)
parseCommand _ = putStrLn "usage: routing-artifact component-transform-report|tile-static-report|export-routing-static [PATH]" >> exitFailure

writeComponentTransformReport :: TileAStar -> IO ()
writeComponentTransformReport astar = do
  createDirectoryIfMissing True "out"
  let rows = componentTransformRows (topologyRoutingComponents (tileTopology astar))
      csvPath = "out/component-transform-report.csv"
      mdPath = "out/component-transform-report.md"
  writeFile csvPath (componentTransformCsv rows)
  writeFile mdPath (componentTransformMarkdown rows)
  printf "wrote %s and %s (%d components, %d bbox cells, %d walkable tiles)\n" csvPath mdPath
    (length rows) (sum (map componentTransformArea rows)) (sum (map componentTransformTiles rows))

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

writeRoutingStaticReport :: FilePath -> TileAStar -> IO ()
writeRoutingStaticReport path astar = do
  (bytes, artifact) <- writeRoutingStaticV1 path astar
  let searchCount = Vector.length (artifactSearchTiles artifact)
      siteCount = Vector.length (artifactSiteTiles artifact)
      componentCount = artifactRoutingComponentCount artifact
      siteComponentValues = Vector.length (artifactSiteComponentIds artifact)
      componentSiteValues = Vector.length (artifactComponentSiteIds artifact)
      bankCount = Vector.length (artifactReachableBankTiles artifact)
      crossingCount = Vector.length (artifactCrossingFromSite artifact)
      sparseOriginals = artifactSparseOriginalCount artifact
      sparseSteiners = artifactSparseSteinerCount artifact
      sparseEdges = artifactSparseUndirectedEdgeCount artifact
      sparseAdjacency = artifactSparseAdjacencyCount artifact
  printf "routing static v1 written: %s\n" path
  printf "bytes: %d\n" bytes
  printf "search tiles: %d\n" searchCount
  printf "sites: %d\n" siteCount
  printf "routing component groups: %d\n" componentCount
  printf "site-component attachments: %d\n" siteComponentValues
  printf "component-site attachments: %d\n" componentSiteValues
  printf "reachable banks: %d\n" bankCount
  printf "separator crossings: %d\n" crossingCount
  printf "sparse original vertices: %d\n" sparseOriginals
  printf "sparse Steiner vertices: %d\n" sparseSteiners
  printf "sparse undirected edges: %d\n" sparseEdges
  printf "sparse adjacency entries: %d\n" sparseAdjacency

data ComponentTransformRow = ComponentTransformRow
  { componentTransformId :: !Int, componentTransformPlane :: !Int
  , componentTransformMinX :: !Int, componentTransformMinY :: !Int
  , componentTransformMaxX :: !Int, componentTransformMaxY :: !Int
  , componentTransformWidth :: !Int, componentTransformHeight :: !Int
  , componentTransformArea :: !Int, componentTransformTiles :: !Int
  }

componentTransformRows :: NaturalComponents -> [ComponentTransformRow]
componentTransformRows components = map row (filter (not . Vector.null . snd) (Boxed.toList (Boxed.indexed groups)))
 where
  groups = componentTileGroups components
  row (cid, tiles) =
    let box = componentBox tiles
        width = boxMaxX box - boxMinX box + 1
        height = boxMaxY box - boxMinY box + 1
     in ComponentTransformRow cid (boxPlane box) (boxMinX box) (boxMinY box) (boxMaxX box) (boxMaxY box) width height (width * height) (Vector.length tiles)

componentTransformCsv :: [ComponentTransformRow] -> String
componentTransformCsv rows = unlines (header : map csvRow rows)
 where
  header = "component,plane,min_x,min_y,max_x,max_y,width,height,bbox_cells,walkable_tiles,fill_ratio"
  csvRow row = joinComma [show (componentTransformId row), show (componentTransformPlane row), show (componentTransformMinX row), show (componentTransformMinY row), show (componentTransformMaxX row), show (componentTransformMaxY row), show (componentTransformWidth row), show (componentTransformHeight row), show (componentTransformArea row), show (componentTransformTiles row), printf "%.6f" (fillRatio row)]
  joinComma = foldr (\value rest -> if null rest then value else value <> "," <> rest) ""

componentTransformMarkdown :: [ComponentTransformRow] -> String
componentTransformMarkdown rows = unlines ["# Component Transform Report", "", "Components: " <> show totalComponents, "Walkable tiles: " <> show totalTiles, "Bounding-box cells: " <> show totalArea, "BBox/walkable multiplier: " <> printf "%.2f" areaMultiplier, "", "## Largest Bounding Boxes", "", table (take 30 (sortOn (Down . componentTransformArea) rows)), "", "## Sparsest Bounding Boxes", "", table (take 30 (sortOn fillRatio rows))]
 where
  totalComponents = length rows
  totalTiles = sum (map componentTransformTiles rows)
  totalArea = sum (map componentTransformArea rows)
  areaMultiplier = fromIntegral totalArea / fromIntegral (max 1 totalTiles) :: Double
  table selected = unlines ("| component | plane | bbox | bbox cells | walkable tiles | fill |" : "|---:|---:|---|---:|---:|---:|" : map tableRow selected)
  tableRow row = "| " <> show (componentTransformId row) <> " | " <> show (componentTransformPlane row) <> " | " <> show (componentTransformMinX row) <> "," <> show (componentTransformMinY row) <> ".." <> show (componentTransformMaxX row) <> "," <> show (componentTransformMaxY row) <> " | " <> show (componentTransformArea row) <> " | " <> show (componentTransformTiles row) <> " | " <> printf "%.4f" (fillRatio row) <> " |"

fillRatio :: ComponentTransformRow -> Double
fillRatio row = fromIntegral (componentTransformTiles row) / fromIntegral (max 1 (componentTransformArea row))

timedPhase :: String -> IO a -> IO a
timedPhase label action = do
  putStrLn ("routing artifact: " <> label)
  hFlush stdout
  started <- getMonotonicTimeNSec
  result <- action
  finished <- getMonotonicTimeNSec
  printf "routing artifact: %s completed in %.1f ms\n" label (milliseconds started finished)
  hFlush stdout
  pure result

milliseconds :: Word64 -> Word64 -> Double
milliseconds started finished = fromIntegral (finished - started) / 1000000

loadOrBuildTileAStar :: World -> IO TileAStar
loadOrBuildTileAStar world = do
  fresh <- tileComponentCacheIsFresh
  cached <- if fresh then loadTileComponentCache else pure Nothing
  case cached of
    Just (components, static) -> case worldTopologyFromComponents productionStructuralReachabilityPolicy world components of
      Left err -> fail (renderReachabilityError world err)
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
  if not exists then pure False else do
    inputs <- concat <$> mapM filesBelow tileComponentCacheInputRoots
    cacheTime <- getModificationTime tileComponentCachePath
    and <$> mapM (fmap (<= cacheTime) . getModificationTime) inputs

filesBelow :: FilePath -> IO [FilePath]
filesBelow path = do
  directory <- doesDirectoryExist path
  if not directory then pure [path] else do
    entries <- map (path </>) <$> listDirectory path
    files <- filterM doesFileExist entries
    directories <- filterM doesDirectoryExist entries
    nested <- concat <$> mapM filesBelow directories
    pure (files <> nested)

tileComponentCacheVersion :: Word64
tileComponentCacheVersion = 15

tileComponentCachePath :: FilePath
tileComponentCachePath = "out/tile-astar-components.bin"

tileComponentCacheInputRoots :: [FilePath]
tileComponentCacheInputRoots = [resourcesDir defaultSourcePaths, "src/ShortestPath/Transport.hs", "src/ShortestPath/Exact/TileAStar.hs", "src/ShortestPath/Exact/TileAStar", "src/ShortestPath/Topology.hs", "src/ShortestPath/Tile.hs", "src/ShortestPath/World.hs"]
