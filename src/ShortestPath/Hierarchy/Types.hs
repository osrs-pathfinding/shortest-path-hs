{-# LANGUAGE DeriveAnyClass #-}

module ShortestPath.Hierarchy.Types
  ( Node(..)
  , TerminalKind(..)
  , TerminalRoles(..)
  , LeafStats(..)
  , LeafOverlay(..)
  , Hierarchy(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Binary (Binary)
import GHC.Generics (Generic)

import ShortestPath.Hierarchy.Partition (LeafId, Partition)
import ShortestPath.Tile (Tile)

data Node
  = Terminal Tile
  | Separator Tile
  | GlobalTeleportHub
  | QuerySource
  | QueryTarget
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Binary)

data TerminalKind
  = BankTerminal
  | LocalTransportOrigin
  | LocalTransportDestination
  | GlobalTeleportDestination
  | RegionGateway
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Binary)

data TerminalRoles = TerminalRoles
  { roleBanks :: Set.Set Tile
  , roleLocalOrigins :: Set.Set Tile
  , roleLocalDestinations :: Set.Set Tile
  , roleGlobalDestinations :: Set.Set Tile
  }
  deriving stock (Eq, Show)

data LeafStats = LeafStats
  { leafTileCount :: Int
  , leafTerminalCount :: Int
  , leafGatewayCount :: Int
  , leafBfsCount :: Int
  , leafExpandedTiles :: Int
  , leafDistanceEntries :: Int
  , leafElapsedMilliseconds :: Integer
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data LeafOverlay = LeafOverlay
  { leafTerminals :: Map.Map Tile (Set.Set TerminalKind)
  , leafDistances :: Map.Map (Tile, Tile) Int
  , leafTerminalAdjacency :: Map.Map Tile (Map.Map Tile Int)
  , leafStats :: LeafStats
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)

data Hierarchy = Hierarchy
  { hierarchyPartition :: Partition
  , leafOverlays :: Map.Map LeafId LeafOverlay
  , terminalLeaf :: Map.Map Tile LeafId
  , hierarchySeparatorNodes :: Map.Map Tile Node
  , hierarchyStats :: Map.Map LeafId LeafStats
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary)
