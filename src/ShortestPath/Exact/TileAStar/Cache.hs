{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ShortestPath.Exact.TileAStar.Cache
  ( loadOrBuildTileAStar
  ) where

import Control.Monad (filterM)
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.FilePath ((</>))

import ShortestPath.Exact.TileAStar
import ShortestPath.Exact.TileAStar.Types (TileAStar(..), TileStatic)
import ShortestPath.Topology
import ShortestPath.Transport (defaultSourcePaths, resourcesDir)
import ShortestPath.World (World)

data TileComponentCache = TileComponentCache Word64 NaturalComponents TileStatic
  deriving stock (Generic)
  deriving anyclass (Binary)

loadOrBuildTileAStar :: World -> IO TileAStar
loadOrBuildTileAStar world = do
  fresh <- tileComponentCacheIsFresh
  cached <- if fresh then loadTileComponentCache else pure Nothing
  case cached of
    Just (components, static) -> do
      topology <- either (fail . renderReachabilityError world) pure
        (worldTopologyFromComponents productionStructuralReachabilityPolicy world components)
      forceTileAStar (TileAStar topology static)
    Nothing -> do
      tileAStar@(TileAStar topology static) <- buildTileAStar world
      createDirectoryIfMissing True "out"
      encodeFile tileComponentCachePath
        (TileComponentCache tileComponentCacheVersion (topologyNaturalComponents topology) static)
      pure tileAStar

loadTileComponentCache :: IO (Maybe (NaturalComponents, TileStatic))
loadTileComponentCache = do
  decoded <- decodeFileOrFail tileComponentCachePath
  pure $ case decoded of
    Right (TileComponentCache version components static)
      | version == tileComponentCacheVersion -> Just (components, static)
    _ -> Nothing

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
tileComponentCacheInputRoots =
  [ resourcesDir defaultSourcePaths
  , "src/ShortestPath/Transport.hs"
  , "src/ShortestPath/Exact/TileAStar.hs"
  , "src/ShortestPath/Exact/TileAStar"
  , "src/ShortestPath/Topology.hs"
  , "src/ShortestPath/Tile.hs"
  , "src/ShortestPath/World.hs"
  ]
