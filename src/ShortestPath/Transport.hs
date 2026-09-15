module ShortestPath.Transport
  ( TransportType(..)
  , Transport(..)
  , SourcePaths(..)
  , defaultSourcePaths
  , transportTypes
  , loadTransports
  , loadBanks
  , parseTileField
  ) where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import System.FilePath ((</>))

import ShortestPath.Requirements
import ShortestPath.Tile
import ShortestPath.Tsv

data TransportType = TransportType
  { ttName :: String
  , ttFile :: FilePath
  , ttIsTeleport :: Bool
  , ttRadius :: Int
  }
  deriving stock (Eq, Ord, Show)

data Transport = Transport
  { transportType :: String
  , origin :: Maybe Tile
  , destination :: Maybe Tile
  , duration :: Int
  , displayInfo :: String
  , objectInfo :: String
  , consumable :: Bool
  , maxWildernessLevel :: Maybe Int
  , skills :: [SkillReq]
  , items :: Maybe ItemExpr
  , quests :: [String]
  , varbits :: [VarReq]
  , varPlayers :: [VarReq]
  , source :: FilePath
  }
  deriving stock (Eq, Ord, Show)

data SourcePaths = SourcePaths
  { resourcesDir :: FilePath
  , collisionZip :: FilePath
  , bankFile :: FilePath
  , separatorFile :: FilePath
  }
  deriving stock (Eq, Ord, Show)

defaultSourcePaths :: SourcePaths
defaultSourcePaths =
  SourcePaths
    { resourcesDir = "/home/matt/shortest-path/src/main/resources"
    , collisionZip = "/home/matt/shortest-path/src/main/resources/collision-map.zip"
    , bankFile = "/home/matt/shortest-path/src/main/resources/destinations/game_features/bank.tsv"
    , separatorFile = "/home/matt/shortest-path/src/main/resources/routing-separators-v1.json"
    }

transportTypes :: [TransportType]
transportTypes =
  [ t "TRANSPORT" "transports.tsv" False 0
  , t "AGILITY_SHORTCUT" "agility_shortcuts.tsv" False 0
  , t "BOAT" "boats.tsv" False 0
  , t "CANOE" "canoes.tsv" False 0
  , t "CHARTER_SHIP" "charter_ships.tsv" False 0
  , t "SHIP" "ships.tsv" False 0
  , t "FAIRY_RING" "fairy_rings.tsv" False 6
  , t "GNOME_GLIDER" "gnome_gliders.tsv" False 6
  , t "HOT_AIR_BALLOON" "hot_air_balloons.tsv" False 7
  , t "MAGIC_CARPET" "magic_carpets.tsv" False 0
  , t "MAGIC_MUSHTREE" "magic_mushtrees.tsv" False 5
  , t "MINECART" "minecarts.tsv" False 0
  , t "QUETZAL" "quetzals.tsv" False 5
  , t "QUETZAL_WHISTLE" "quetzal_whistle.tsv" True 0
  , t "SEASONAL_TRANSPORTS" "seasonal_transports.tsv" False 0
  , t "SPIRIT_TREE" "spirit_trees.tsv" False 5
  , t "TELEPORTATION_BOX" "teleportation_boxes.tsv" False 0
  , t "TELEPORTATION_ITEM" "teleportation_items.tsv" True 0
  , t "TELEPORTATION_LEVER" "teleportation_levers.tsv" False 0
  , t "TELEPORTATION_MINIGAME" "teleportation_minigames.tsv" True 0
  , t "TELEPORTATION_PORTAL" "teleportation_portals.tsv" False 0
  , t "TELEPORTATION_PORTAL_POH" "teleportation_portals_poh.tsv" False 0
  , t "TELEPORTATION_SPELL" "teleportation_spells.tsv" True 0
  , t "TELEPORTATION_SPELL_HOME" "teleportation_spells_home.tsv" True 0
  , t "WILDERNESS_OBELISK" "wilderness_obelisks.tsv" False 0
  ]
 where
  t n f tel r = TransportType n f tel r

loadTransports :: SourcePaths -> IO [Transport]
loadTransports paths = concat <$> forM transportTypes (loadType paths)

loadBanks :: SourcePaths -> IO [Tile]
loadBanks paths = catMaybes . map (parseTileField . field "Destination") <$> readRows (bankFile paths)

loadType :: SourcePaths -> TransportType -> IO [Transport]
loadType paths tt = do
  let path = resourcesDir paths </> "transports" </> ttFile tt
  rows <- zip [2 :: Int ..] <$> readRows path
  raw <- either fail pure (mapM (uncurry (fromRow tt path)) rows)
  let
      direct = filter isDirect raw
      origins = filter isOriginOnly raw
      destinations = filter isDestinationOnly raw
      permuted = [mergeTransport a b | a <- origins, b <- destinations, farEnough a b]
  pure (direct <> permuted)
 where
  isDirect tr = case (origin tr, destination tr) of
    (Just a, Just b) -> a /= b
    (Nothing, Just _) -> ttIsTeleport tt || ttName tt == "SEASONAL_TRANSPORTS"
    _ -> False
  isOriginOnly tr = origin tr /= Nothing && destination tr == Nothing
  isDestinationOnly tr = origin tr == Nothing && destination tr /= Nothing
  farEnough a b =
    case (origin a, destination b) of
      (Just x, Just y) -> maybe False (> ttRadius tt) (chebyshev2 x y)
      _ -> False

fromRow :: TransportType -> FilePath -> Int -> Row -> Either String Transport
fromRow tt path lineNo r =
  case parseItems (field "Items" r) of
    Left resolutionError -> Left (path <> ":" <> show lineNo <> ": item requirement: " <> show resolutionError)
    Right itemRequirements -> Right Transport
      { transportType = ttName tt
      , origin = parseTileField (field "Origin" r)
      , destination = parseTileField (field "Destination" r)
      , duration = max teleportMinimum (parseInt 0 (field "Duration" r))
      , displayInfo = field "Display info" r
      , objectInfo = field "menuOption menuTarget objectID" r
      , consumable = field "Consumable" r `elem` ["T", "yes", "YES"]
      , maxWildernessLevel = parseMaybeInt (field "Wilderness level" r)
      , skills = parseSkills (field "Skills" r)
      , items = itemRequirements
      , quests = parseQuests (field "Quests" r)
      , varbits = parseVars Varbit (field "Varbits" r)
      , varPlayers = parseVars VarPlayer (field "VarPlayers" r)
      , source = path <> ":" <> show lineNo
      }
 where
  teleportMinimum = if ttIsTeleport tt then 1 else 0

mergeTransport :: Transport -> Transport -> Transport
mergeTransport a b =
  a
    { destination = destination b
    , duration = max (duration a) (duration b)
    , displayInfo = displayInfo b
    , consumable = consumable a || consumable b
    , maxWildernessLevel = maxMaybe (maxWildernessLevel a) (maxWildernessLevel b)
    , skills = mergeDistinct (skills a <> skills b)
    , items = mergeItems (items a) (items b)
    , quests = mergeDistinct (quests a <> quests b)
    , varbits = mergeDistinct (varbits a <> varbits b)
    , varPlayers = mergeDistinct (varPlayers a <> varPlayers b)
    , source = source a <> " + " <> source b
    }

parseTileField :: String -> Maybe Tile
parseTileField raw =
  case words raw of
    [x, y, p] -> packTile <$> readMaybe x <*> readMaybe y <*> readMaybe p
    _ -> Nothing

parseMaybeInt :: String -> Maybe Int
parseMaybeInt "" = Nothing
parseMaybeInt s = readMaybe s

parseInt :: Int -> String -> Int
parseInt def s = maybe def id (readMaybe s)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing b = b
maxMaybe a Nothing = a
maxMaybe (Just a) (Just b) = Just (max a b)

mergeItems :: Maybe ItemExpr -> Maybe ItemExpr -> Maybe ItemExpr
mergeItems Nothing b = b
mergeItems a Nothing = a
mergeItems (Just a) (Just b) = Just (ItemAnd [a, b])

mergeDistinct :: Ord a => [a] -> [a]
mergeDistinct = Map.keys . Map.fromList . map (, ())
