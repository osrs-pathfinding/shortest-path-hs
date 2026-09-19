{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as Vector
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, lookupEnv)
import System.IO (IOMode(WriteMode), hFlush, hPutStrLn, stdout, withFile)
import System.Process (callProcess)
import Text.Read (readMaybe)

import ShortestPath.Separator
import ShortestPath.Tile
import ShortestPath.Topology
  ( NaturalComponents(..), StructuralReachability(..), WorldTopology(..)
  , naturalComponents, productionStructuralReachabilityPolicy
  , walkingTopologyIdentity, withEmptySeparatorArtifact, worldTopologyFromComponents
  )
import ShortestPath.Transport (SourcePaths(..), defaultSourcePaths)
import ShortestPath.World

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["generate", output, maximumText, minimumText, maximumSeparatorText, imbalanceText, seedText]
      | Just maximumSize <- readMaybe maximumText
      , Just minimumSize <- readMaybe minimumText
      , Just maximumSeparator <- readMaybe maximumSeparatorText
      , Just imbalance <- readMaybe imbalanceText
      , Just seed <- readMaybe seedText
      , maximumSize >= 2 * minimumSize
      , minimumSize > 0
      , maximumSeparator >= 0
      , imbalance >= 0 -> generateArtifact output (SeparatorConfig maximumSize minimumSize maximumSeparator imbalance "strong" seed)
    _ -> putStrLn "usage: separator-artifact generate OUTPUT MAXIMUM-SIZE MINIMUM-CHILD MAXIMUM-SEPARATOR IMBALANCE SEED"

loadConfiguredWorld :: IO World
loadConfiguredWorld = do
  resources <- lookupEnv "SPM_RESOURCES_DIR"
  collision <- lookupEnv "SPM_COLLISION_ZIP"
  bank <- lookupEnv "SPM_BANK_FILE"
  loadWorldWithoutSeparators defaultSourcePaths
    { resourcesDir = maybe (resourcesDir defaultSourcePaths) id resources
    , collisionZip = maybe (collisionZip defaultSourcePaths) id collision
    , bankFile = maybe (bankFile defaultSourcePaths) id bank
    }

generateArtifact :: FilePath -> SeparatorConfig -> IO ()
generateArtifact output config = do
  createDirectoryIfMissing True "out/kahip-separators"
  world <- timed "load world without separators" loadConfiguredWorld
  natural <- timed "authoritative natural walking components" (naturalComponents world)
  let grouped = Map.toAscList (Map.fromListWith (<>)
        [(component, [tile]) | (tile, component) <- zip (Vector.toList (componentOwnerTiles natural)) (Vector.toList (componentOwnerIds natural))])
  baseline <- either (fail . show) pure
    (worldTopologyFromComponents productionStructuralReachabilityPolicy (withEmptySeparatorArtifact world) natural)
  let reachable = structurallyReachableIds (topologyStructuralReachability baseline)
      aboveThreshold = filter ((> separatorMaximumComponentSize config) . length . snd) grouped
      selected = separatorCandidates (separatorMaximumComponentSize config) reachable grouped
      totalTiles = sum (map (length . snd) selected)
  putFlush ("natural_components_total=" <> show (length grouped))
  putFlush ("structurally_reachable_components=" <> show (IntSet.size reachable))
  putFlush ("components_above_size_threshold=" <> show (length aboveThreshold))
  putFlush ("reachable_components_above_size_threshold=" <> show (length selected))
  putFlush ("kahip_top_level_components_attempted=" <> show (length selected))
  putFlush ("kahip_tiles_considered=" <> show totalTiles)
  putFlush ("KaHIP top-level components: " <> intercalate ", " [show component <> " (" <> show (length tiles) <> ")" | (component, tiles) <- selected])
  assignments <- fmap IntMap.unions (forM selected $ \(component, tiles) -> do
    putFlush ("partitioning natural component " <> show component <> " (" <> show (length tiles) <> " tiles)")
    partitionComponent world config component "1" 0 tiles)
  let cuts = Set.toAscList (Set.fromList
        [ canonicalCut (Tile from) to
        | (from, fromRegion) <- IntMap.toAscList assignments
        , to <- walkingNeighbors world (Tile from)
        , from < unTile to
        , Just toRegion <- [IntMap.lookup (unTile to) assignments]
        , fromRegion /= toRegion
        ])
      artifact = SeparatorArtifact separatorArtifactVersion (walkingTopologyIdentity world) config cuts
  BL.writeFile output (encode artifact)
  BL.writeFile (output <> ".diagnostics.json") (encode (object
    [ "natural_components_total" .= length grouped
    , "structurally_reachable_components" .= IntSet.size reachable
    , "components_above_size_threshold" .= length aboveThreshold
    , "reachable_components_above_size_threshold" .= length selected
    , "kahip_top_level_components_attempted" .= length selected
    , "kahip_tiles_considered" .= totalTiles
    , "top_level_components" .= [object ["component_id" .= component, "tile_count" .= length tiles] | (component, tiles) <- selected]
    ]))
  putStrLn ("wrote " <> output <> " with " <> show (length cuts) <> " cut walking edges and generation diagnostics")

partitionComponent :: World -> SeparatorConfig -> Int -> String -> Int -> [Int] -> IO (IntMap.IntMap String)
partitionComponent world config component label level tiles
  | length tiles <= separatorMaximumComponentSize config = pure (owned ("leaf-" <> show component <> "-" <> label) tiles)
  | length tiles < 2 * separatorMinimumChildSize config = reject "cannot meet minimum child size"
  | otherwise = do
      let stem = "out/kahip-separators/component-" <> show component <> "-" <> label
          graph = stem <> ".graph"
          result = stem <> ".separator"
      writeGraph world tiles graph
      callProcess "node_separator"
        [ graph
        , "--output_filename=" <> result
        , "--seed=" <> show (separatorRandomSeed config)
        , "--imbalance=" <> show (separatorImbalance config)
        , "--preconfiguration=" <> separatorPreconfiguration config
        ]
      parts <- readParts result (length tiles)
      let leftTiles = [tile | (tile, 0) <- zip tiles parts]
          rightTiles = [tile | (tile, 1) <- zip tiles parts]
          separatorTiles = [tile | (tile, 2) <- zip tiles parts]
      case separatorRejection config (length leftTiles) (length rightTiles) (length separatorTiles) of
        Just reason -> reject reason
        Nothing -> do
          left <- partitionComponent world config component (label <> "a") (level + 1) leftTiles
          right <- partitionComponent world config component (label <> "b") (level + 1) rightTiles
          pure (IntMap.unions [owned ("separator-" <> show component <> "-" <> label <> "-" <> show level) separatorTiles, left, right])
 where
  owned region = IntMap.fromList . map (, region)
  reject reason = do
    putFlush ("reject component " <> show component <> " region " <> label <> " (" <> show (length tiles) <> " tiles): " <> reason)
    pure (owned ("leaf-" <> show component <> "-" <> label) tiles)

readParts :: FilePath -> Int -> IO [Int]
readParts path expected = do
  contents <- readFile path
  parts <- mapM parsePart (lines contents)
  if length parts == expected then pure parts else fail ("partition output length mismatch: " <> path)
 where
  parsePart value = case readMaybe value of
    Just part | part `elem` [0 :: Int, 1, 2] -> pure part
    _ -> fail ("invalid partition value in " <> path)

writeGraph :: World -> [Int] -> FilePath -> IO ()
writeGraph world tiles path = withFile path WriteMode $ \handle -> do
  let ids = IntMap.fromList (zip tiles [1 :: Int ..])
      neighbours packed = [vertex | tile <- walkingNeighbors world (Tile packed), Just vertex <- [IntMap.lookup (unTile tile) ids]]
      edgeCount = sum (map (length . neighbours) tiles) `div` 2
  hPutStrLn handle (show (length tiles) <> " " <> show edgeCount)
  forM_ tiles (hPutStrLn handle . unwords . map show . neighbours)

timed :: String -> IO a -> IO a
timed label action = putFlush ("start: " <> label) >> action >>= \result -> putFlush ("done: " <> label) >> pure result

putFlush :: String -> IO ()
putFlush message = putStrLn message >> hFlush stdout
