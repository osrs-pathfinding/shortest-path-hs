{-# LANGUAGE OverloadedStrings #-}

module ShortestPath.BenchmarkCorpus
  ( BenchmarkRoute(..)
  , loadBenchmarkRoutes
  , loadBenchmarkRoutesFrom
  ) where

import Data.Aeson (FromJSON(..), eitherDecodeFileStrict', withObject, (.:), (.:?), (.!=))
import System.FilePath ((</>))

data BenchmarkRoute = BenchmarkRoute
  { routeId :: Maybe String
  , routeName :: String
  , routeCategory :: String
  , routeDistanceTag :: String
  , routePlaneTag :: String
  , routeStart :: [Int]
  , routeTarget :: [Int]
  , routeAllowTransports :: Bool
  , routeTiers :: [String]
  , routeNegativeProfiles :: [String]
  }

instance FromJSON BenchmarkRoute where
  parseJSON = withObject "benchmark route" $ \v ->
    BenchmarkRoute <$> v .:? "id" <*> v .: "name" <*> v .:? "category" .!= "unknown"
      <*> v .:? "distanceTag" .!= "unknown" <*> v .:? "planeTag" .!= "unknown"
      <*> v .: "start" <*> v .: "target" <*> v .: "allowTransports"
      <*> v .:? "tiers" .!= [] <*> v .:? "negativeProfiles" .!= []

loadBenchmarkRoutes :: FilePath -> IO [BenchmarkRoute]
loadBenchmarkRoutes root = loadBenchmarkRoutesFrom (root </> "corpus/routes-v1.json")

loadBenchmarkRoutesFrom :: FilePath -> IO [BenchmarkRoute]
loadBenchmarkRoutesFrom path = either fail pure =<< eitherDecodeFileStrict' path
