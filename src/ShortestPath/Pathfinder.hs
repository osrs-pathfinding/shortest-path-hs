module ShortestPath.Pathfinder
  ( Query(..)
  , BankItems(..)
  , ItemCounts
  , Route(..)
  , RouteStep(..)
  , RouteFinder(..)
  , defaultQuery
  , transportAvailable
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Requirements
import ShortestPath.Tile
import ShortestPath.Transport (Transport(..), TransportType(..), transportTypes)

type ItemCounts = Map.Map String Int

data BankItems = AllBankItems | BankItems ItemCounts
  deriving stock (Eq, Show)

data Query = Query
  { queryStart :: Tile
  , queryTarget :: Tile
  , allowTransports :: Bool
  , enabledTransportTypes :: Set.Set String
  , transportPenalties :: Map.Map String Int
  , bankPathEnabled :: Bool
  , heuristicWeight :: Double
  , inventoryItems :: ItemCounts
  , bankItems :: BankItems
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
    , enabledTransportTypes = Set.fromList
        [ ttName transportType
        | transportType <- transportTypes
        , ttName transportType /= "SEASONAL_TRANSPORTS"
        ]
    , transportPenalties = Map.empty
    , bankPathEnabled = True
    , heuristicWeight = 1
    , inventoryItems = Map.fromList [("772", 1), ("8007", 1), ("13393", 1)]
    , bankItems = AllBankItems
    }

transportAvailable :: Query -> Bool -> Transport -> Bool
transportAvailable query banked transport =
  enabled && maybe True (itemsAvailable query banked) (items transport)
 where
  enabled =
    Set.null (enabledTransportTypes query)
      || Set.member (transportType transport) (enabledTransportTypes query)

itemsAvailable :: Query -> Bool -> ItemExpr -> Bool
itemsAvailable query banked expr =
  case expr of
    ItemOne term -> termAvailable query banked term
    ItemAnd terms -> all (itemsAvailable query banked) terms
    ItemOr terms -> any (itemsAvailable query banked) terms

termAvailable :: Query -> Bool -> ItemTerm -> Bool
termAvailable query banked (ItemTerm name quantity)
  | quantity <= 0 = Map.findWithDefault 0 name (inventoryItems query) <= 0
  | hasItem (inventoryItems query) name quantity = True
  | banked =
      case bankItems query of
        AllBankItems -> True
        BankItems counts -> hasItem counts name quantity
  | otherwise = False

hasItem :: ItemCounts -> String -> Int -> Bool
hasItem counts name quantity = Map.findWithDefault 0 name counts >= quantity
