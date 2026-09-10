{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Hashable (hash)
import Control.Monad (forM)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

import ShortestPath.Exact.TileAStar
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.Tsv
import ShortestPath.World

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["load-summary"] -> do
      world <- loadWorld defaultSourcePaths
      printf "regions: %d\n" (Map.size (collisionRegions (worldCollision world)))
      printf "banks: %d\n" (Set.size (worldBanks world))
      printf "local transport origins: %d\n" (Map.size (worldTransports world))
      printf "global teleports: %d\n" (length (worldGlobalTeleports world))
    ["route", sx, sy, sp, tx, ty, tp] -> do
      world <- loadWorld defaultSourcePaths
      pathfinder <- buildTileAStar world
      let q = defaultQuery (packTile (read sx) (read sy) (read sp)) (packTile (read tx) (read ty) (read tp))
          r = findRoute pathfinder q
      LBS.putStrLn (encode (routeJson r))
    ["walk-route", sx, sy, sp, tx, ty, tp] -> do
      world <- loadWorld defaultSourcePaths
      pathfinder <- buildTileAStar world
      let q = (defaultQuery (packTile (read sx) (read sy) (read sp)) (packTile (read tx) (read ty) (read tp))) {allowTransports = False}
          r = findRoute pathfinder q
      LBS.putStrLn (encode (routeJson r))
    ["dashboard-json"] -> do
      items <- loadDashboardItems defaultSourcePaths
      LBS.putStrLn (encode (map dashboardItemJson items))
    _ -> putStrLn "usage: shortest-path-model load-summary | route sx sy sp tx ty tp | walk-route sx sy sp tx ty tp | dashboard-json"

routeJson :: Route -> Value
routeJson r =
  object
    [ "cost" .= routeCost r
    , "expandedNodes" .= routeExpandedNodes r
    , "path" .= map stepJson (routeSteps r)
    ]

stepJson :: RouteStep -> Value
stepJson (Walk t) = object ["kind" .= ("walk" :: String), "coordinate" .= coordinateText t]
stepJson (UseTransport name t) = object ["kind" .= ("transport" :: String), "label" .= name, "coordinate" .= coordinateText t]

type DashboardItem = (Tile, [(String, String)])

loadDashboardItems :: SourcePaths -> IO [DashboardItem]
loadDashboardItems paths = do
  transportItems <- concat <$> forM transportTypes loadTransportPoints
  bankItems <- map (, [("Destination: Bank", "destinations/game_features/bank.tsv")]) <$> loadBanks paths
  pure (Map.toList (Map.fromListWith mergeEntries (transportItems <> bankItems)))
 where
  loadTransportPoints tt = do
    let path = resourcesDir paths </> "transports" </> ttFile tt
    rows <- readRows path
    pure
      [ (tile, [(label, path)])
      | r <- rows
      , let display = field "Display info" r
      , let objectInfoText = field "menuOption menuTarget objectID" r
      , (role, col) <- [("origin", "Origin"), ("destination", "Destination")]
      , Just tile <- [parseTileField (field col r)]
      , let label = ttName tt <> ": " <> role <> labelSuffix display objectInfoText
      ]

dashboardItemJson :: DashboardItem -> Value
dashboardItemJson (tile, entries) =
  object
    [ "id" .= ("coord-" <> show (abs (hash (coordinateText tile <> "|" <> concatMap fst entries))))
    , "entries" .= [object ["label" .= label, "source" .= src] | (label, src) <- entries]
    , "coordinate" .= coordinateText tile
    ]

labelSuffix :: String -> String -> String
labelSuffix display objectInfoText
  | null display && null objectInfoText = ""
  | null display = " - " <> objectInfoText
  | null objectInfoText = " - " <> display
  | otherwise = " - " <> display <> " / " <> objectInfoText

mergeEntries :: [(String, String)] -> [(String, String)] -> [(String, String)]
mergeEntries a b = Map.keys (Map.fromList (map (, ()) (a <> b)))
