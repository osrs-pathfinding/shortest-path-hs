module ShortestPath.Pathfinder
  ( Query(..)
  , RoutingOptions(..)
  , SearchOptions(..)
  , PreparedRoutingAccount(..)
  , EffectiveRoutingFingerprint
  , QueryTransportAvailability(..)
  , routingOptionsFromQuery
  , searchOptionsFromQuery
  , prepareRoutingAccount
  , queryRequirementContext
  , prepareQueryTransports
  , preparedLocalTransportsAt
  , preparedGlobalTransports
  , preparedWildernessGlobalTransports
  , preparedTransport
  , transportLabel
  , bankTransitionAvailable
  , Route(..)
  , RouteStep(..)
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

data RoutingOptions = RoutingOptions
  { routingAllowTransports :: Bool
  , routingEnabledTransportTypes :: Set.Set String
  , routingTransportPenalties :: Map.Map String Int
  , routingBankPathEnabled :: Bool
  , routingRequirementMode :: RequirementMode
  , routingNowMinutes :: Int
  }
  deriving stock (Eq, Show)

newtype SearchOptions = SearchOptions
  { searchHeuristicWeight :: Double
  }
  deriving stock (Eq, Show)

data QueryTransportAvailability = QueryTransportAvailability
  { carriedLocalTransports :: Map.Map Tile [Transport]
  , bankedLocalTransports :: Map.Map Tile [Transport]
  , carriedGlobalTransports :: [Transport]
  , bankedGlobalTransports :: [Transport]
  , carriedWildernessGlobalTransports :: [Transport]
  , bankedWildernessGlobalTransports :: [Transport]
  }
  deriving stock (Eq, Show)

data EffectiveRoutingFingerprint = EffectiveRoutingFingerprint
  !Bool !Bool !(Map.Map String Int) !QueryTransportAvailability
  deriving stock (Eq, Show)

data PreparedRoutingAccount = PreparedRoutingAccount
  { preparedTransportAvailability :: QueryTransportAvailability
  , preparedAllowTransports :: !Bool
  , preparedTransportPenalties :: Map.Map String Int
  , preparedBankPathEnabled :: !Bool
  , preparedRoutingFingerprint :: EffectiveRoutingFingerprint
  }

data RouteStep = Walk Tile | UseTransport String Tile
  deriving stock (Eq, Show)

data Route = Route
  { routeCost :: Int
  , routeExpandedNodes :: Int
  , routeSteps :: [RouteStep]
  }
  deriving stock (Eq, Show)

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
prepareQueryTransports world = prepareRoutingTransports world . routingOptionsFromQuery

routingOptionsFromQuery :: Query -> RoutingOptions
routingOptionsFromQuery query = RoutingOptions
  (allowTransports query)
  (enabledTransportTypes query)
  (transportPenalties query)
  (bankPathEnabled query)
  (requirementMode query)
  (queryNowMinutes query)

searchOptionsFromQuery :: Query -> SearchOptions
searchOptionsFromQuery = SearchOptions . heuristicWeight

prepareRoutingAccount :: World -> RoutingOptions -> PreparedRoutingAccount
prepareRoutingAccount world options =
  PreparedRoutingAccount availability allow penalties bankEnabled fingerprint
 where
  availability = prepareRoutingTransports world options
  allow = routingAllowTransports options
  bankEnabled = routingBankPathEnabled options
  penalties
    | allow = Map.filter (/= 0) (Map.restrictKeys (routingTransportPenalties options) availableTypes)
    | otherwise = Map.empty
  availableTypes = Set.fromList
    [ transportType transport
    | transport <- concat (Map.elems (carriedLocalTransports availability))
        <> concat (Map.elems (bankedLocalTransports availability))
        <> carriedGlobalTransports availability
        <> bankedGlobalTransports availability
    ]
  effectiveAvailability
    | allow = availability
    | otherwise = QueryTransportAvailability Map.empty Map.empty [] [] [] []
  fingerprint = EffectiveRoutingFingerprint allow bankEnabled penalties effectiveAvailability

prepareRoutingTransports :: World -> RoutingOptions -> QueryTransportAvailability
prepareRoutingTransports world options =
  QueryTransportAvailability carriedLocals bankedLocals carriedGlobals bankedGlobals
    (filter wildernessCapable carriedGlobals) (filter wildernessCapable bankedGlobals)
 where
  carriedLocals = filterLocals False
  bankedLocals = filterLocals True
  carriedGlobals = filterAvailable False (worldGlobalTeleports world)
  bankedGlobals = filterAvailable True (worldGlobalTeleports world)
  wildernessCapable transport = maxWildernessLevel transport >= Just 30
  filterLocals banked = Map.map (filterAvailable banked) (worldTransports world)
  filterAvailable banked = filter (transportAvailableWithOptions options banked)

preparedLocalTransportsAt :: QueryTransportAvailability -> Bool -> Tile -> [Transport]
{-# INLINE preparedLocalTransportsAt #-}
preparedLocalTransportsAt availability banked tile =
  filter ((/= "VIRTUAL_WALL") . transportType) $ Map.findWithDefault [] tile
    (if banked then bankedLocalTransports availability else carriedLocalTransports availability)

preparedGlobalTransports :: QueryTransportAvailability -> Bool -> [Transport]
{-# INLINE preparedGlobalTransports #-}
preparedGlobalTransports availability banked =
  if banked then bankedGlobalTransports availability else carriedGlobalTransports availability

preparedWildernessGlobalTransports :: QueryTransportAvailability -> Bool -> [Transport]
{-# INLINE preparedWildernessGlobalTransports #-}
preparedWildernessGlobalTransports availability banked =
  if banked then bankedWildernessGlobalTransports availability else carriedWildernessGlobalTransports availability

preparedTransport :: Query -> Transport -> Maybe (Tile, Int, RouteStep)
{-# INLINE preparedTransport #-}
preparedTransport query transport = do
  target <- destination transport
  pure
    ( target
    , duration transport + Map.findWithDefault 0 (transportType transport) (transportPenalties query)
    , UseTransport (transportLabel transport) target
    )

transportLabel :: Transport -> String
transportLabel transport
  | null (displayInfo transport) = transportType transport
  | otherwise = displayInfo transport

bankTransitionAvailable :: Query -> Set.Set Tile -> Bool -> Tile -> Bool
{-# INLINE bankTransitionAvailable #-}
bankTransitionAvailable query banks banked tile =
  bankPathEnabled query && not banked && Set.member tile banks

transportAvailable :: Query -> Bool -> Transport -> Bool
transportAvailable query = transportAvailableWithOptions (routingOptionsFromQuery query)

transportExplanation :: Query -> Bool -> Transport -> TransportAvailability
transportExplanation query = transportExplanationWithOptions (routingOptionsFromQuery query)

transportAvailableWithOptions :: RoutingOptions -> Bool -> Transport -> Bool
transportAvailableWithOptions options banked = (== Available) . transportExplanationWithOptions options banked

transportExplanationWithOptions :: RoutingOptions -> Bool -> Transport -> TransportAvailability
transportExplanationWithOptions options banked transport =
  if not enabled
    then TransportTypeDisabled (transportType transport)
    else case routingRequirementMode options of
      IgnoreRequirements -> Available
      ConfiguredRequirements account -> transportAvailability (routingRequirementContext options account banked) transport
 where
  enabled = Set.null (routingEnabledTransportTypes options) || Set.member (transportType transport) (routingEnabledTransportTypes options)

queryRequirementContext :: Query -> AccountState -> Bool -> RequirementContext
queryRequirementContext query account banked =
  RequirementContext account (if banked then CarriedAndBank else CarriedOnly) (queryNowMinutes query)

routingRequirementContext :: RoutingOptions -> AccountState -> Bool -> RequirementContext
routingRequirementContext options account banked =
  RequirementContext account (if banked then CarriedAndBank else CarriedOnly) (routingNowMinutes options)
