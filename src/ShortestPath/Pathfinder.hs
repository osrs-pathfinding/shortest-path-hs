module ShortestPath.Pathfinder
  ( Query(..)
  , Route(..)
  , RouteStep(..)
  , RouteFinder(..)
  , defaultQuery
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Tile

data Query = Query
  { queryStart :: Tile
  , queryTarget :: Tile
  , allowTransports :: Bool
  , enabledTransportTypes :: Set.Set String
  , transportPenalties :: Map.Map String Int
  , bankPathEnabled :: Bool
  }
  deriving stock (Eq, Show)

data RouteStep = Walk Tile | UseTransport String Tile
  deriving stock (Eq, Show)

data Route = Route
  { routeCost :: Int
  , routeExpandedNodes :: Int
  , routeSteps :: [RouteStep]
  }
  deriving stock (Eq, Show)

class RouteFinder a where
  routeName :: a -> String
  findRoute :: a -> Query -> Route

defaultQuery :: Tile -> Tile -> Query
defaultQuery start target =
  Query
    { queryStart = start
    , queryTarget = target
    , allowTransports = True
    , enabledTransportTypes = Set.empty
    , transportPenalties = Map.empty
    , bankPathEnabled = True
    }
