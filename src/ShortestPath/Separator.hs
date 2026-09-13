{-# LANGUAGE OverloadedStrings #-}

module ShortestPath.Separator
  ( SeparatorArtifact(..)
  , SeparatorConfig(..)
  , SeparatorCut(..)
  , separatorArtifactVersion
  , canonicalCut
  , separatorCandidates
  , separatorRejection
  ) where

import Data.Aeson
import qualified Data.IntSet as IntSet

import ShortestPath.Tile

data SeparatorConfig = SeparatorConfig
  { separatorMaximumComponentSize :: !Int
  , separatorMinimumChildSize :: !Int
  , separatorMaximumSeparatorSize :: !Int
  , separatorImbalance :: !Int
  , separatorPreconfiguration :: !String
  , separatorRandomSeed :: !Int
  }
  deriving stock (Eq, Show)

data SeparatorCut = SeparatorCut !Tile !Tile
  deriving stock (Eq, Ord, Show)

data SeparatorArtifact = SeparatorArtifact
  { separatorFormat :: !String
  , separatorTopologyIdentity :: !String
  , separatorConfiguration :: !SeparatorConfig
  , separatorCuts :: ![SeparatorCut]
  }
  deriving stock (Eq, Show)

separatorArtifactVersion :: String
separatorArtifactVersion = "osrs-routing-separators-v1"

canonicalCut :: Tile -> Tile -> SeparatorCut
canonicalCut a b = SeparatorCut (min a b) (max a b)

separatorCandidates :: Int -> IntSet.IntSet -> [(Int, [a])] -> [(Int, [a])]
separatorCandidates maximumSize reachable =
  filter (\(component, tiles) -> IntSet.member component reachable && length tiles > maximumSize)

separatorRejection :: SeparatorConfig -> Int -> Int -> Int -> Maybe String
separatorRejection config leftSize rightSize separatorSize
  | leftSize < separatorMinimumChildSize config || rightSize < separatorMinimumChildSize config =
      Just ("child below minimum: " <> show leftSize <> "/" <> show rightSize)
  | separatorSize > separatorMaximumSeparatorSize config =
      Just ("separator too large: " <> show separatorSize <> " > " <> show (separatorMaximumSeparatorSize config))
  | otherwise = Nothing

instance ToJSON SeparatorConfig where
  toJSON value = object
    [ "maximumComponentSize" .= separatorMaximumComponentSize value
    , "minimumChildSize" .= separatorMinimumChildSize value
    , "maximumSeparatorSize" .= separatorMaximumSeparatorSize value
    , "imbalance" .= separatorImbalance value
    , "preconfiguration" .= separatorPreconfiguration value
    , "randomSeed" .= separatorRandomSeed value
    ]

instance FromJSON SeparatorConfig where
  parseJSON = withObject "separator configuration" $ \value -> SeparatorConfig
    <$> value .: "maximumComponentSize"
    <*> value .: "minimumChildSize"
    <*> value .: "maximumSeparatorSize"
    <*> value .: "imbalance"
    <*> value .: "preconfiguration"
    <*> value .: "randomSeed"

instance ToJSON SeparatorCut where
  toJSON (SeparatorCut from to) = object ["from" .= tileJson from, "to" .= tileJson to]
   where
    tileJson tile = let (x, y, plane) = unpackTile tile in [x, y, plane]

instance FromJSON SeparatorCut where
  parseJSON = withObject "separator cut" $ \value ->
    canonicalCut <$> (value .: "from" >>= parseTile) <*> (value .: "to" >>= parseTile)
   where
    parseTile [x, y, plane] = pure (packTile x y plane)
    parseTile _ = fail "separator tile must be [x,y,plane]"

instance ToJSON SeparatorArtifact where
  toJSON value = object
    [ "format" .= separatorFormat value
    , "topologyIdentity" .= separatorTopologyIdentity value
    , "configuration" .= separatorConfiguration value
    , "cutEdges" .= separatorCuts value
    ]

instance FromJSON SeparatorArtifact where
  parseJSON = withObject "separator artifact" $ \value -> SeparatorArtifact
    <$> value .: "format"
    <*> value .: "topologyIdentity"
    <*> value .: "configuration"
    <*> value .: "cutEdges"
