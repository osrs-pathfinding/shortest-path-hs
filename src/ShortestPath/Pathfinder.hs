module ShortestPath.Pathfinder
  ( Query(..)
  , QueryTransportAvailability(..)
  , queryRequirementContext
  , prepareQueryTransports
  , Route(..)
  , RouteStep(..)
  , RouteFinder(..)
  , defaultQuery
  , transportAvailable
  , transportExplanation
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ShortestPath.Account
import ShortestPath.Tile
import ShortestPath.Transport (Transport(..), TransportType(..), transportTypes)
import ShortestPath.World (World(..))

data Query = Query
  { queryStart :: Tile
  , queryTarget :: Tile
  , allowTransports :: Bool
  , enabledTransportTypes :: Set.Set String
  , transportPenalties :: Map.Map String Int
  , bankPathEnabled :: Bool
  , heuristicWeight :: Double
  , requirementMode :: RequirementMode
  , queryNowMinutes :: Int
  }
  deriving stock (Eq, Show)

data QueryTransportAvailability = QueryTransportAvailability
  { carriedLocalTransports :: Map.Map Tile [Transport]
  , bankedLocalTransports :: Map.Map Tile [Transport]
  , carriedGlobalTransports :: [Transport]
  , bankedGlobalTransports :: [Transport]
  }
  deriving stock (Show)

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
    , requirementMode = IgnoreRequirements
    , queryNowMinutes = 0
    }

prepareQueryTransports :: World -> Query -> QueryTransportAvailability
prepareQueryTransports world query =
  QueryTransportAvailability
    (filterLocals False)
    (filterLocals True)
    (filterAvailable False (worldGlobalTeleports world))
    (filterAvailable True (worldGlobalTeleports world))
 where
  filterLocals banked = Map.map (filterAvailable banked) (worldTransports world)
  filterAvailable banked = filter (transportAvailable query banked)

transportAvailable :: Query -> Bool -> Transport -> Bool
transportAvailable query banked = (== Available) . transportExplanation query banked

transportExplanation :: Query -> Bool -> Transport -> TransportAvailability
transportExplanation query banked transport =
  if not enabled
    then TransportTypeDisabled (transportType transport)
    else case requirementMode query of
      IgnoreRequirements -> Available
      ConfiguredRequirements account -> transportAvailability (queryRequirementContext query account banked) transport
 where
  enabled = Set.null (enabledTransportTypes query) || Set.member (transportType transport) (enabledTransportTypes query)

queryRequirementContext :: Query -> AccountState -> Bool -> RequirementContext
queryRequirementContext query account banked =
  RequirementContext account (if banked then CarriedAndBank else CarriedOnly) (queryNowMinutes query)
