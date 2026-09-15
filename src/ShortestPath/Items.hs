module ShortestPath.Items
  ( ItemId
  , ItemVariation(..)
  , ItemResolutionError(..)
  , routingItemVariations
  , resolveItemName
  ) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map

type ItemId = Int

newtype ItemVariation = ItemVariation { variationIds :: [ItemId] }
  deriving stock (Eq, Ord, Show)

data ItemResolutionError
  = UnknownItemName String
  | InvalidNumericItemId String
  deriving stock (Eq, Ord, Show)

resolveItemName :: String -> Either ItemResolutionError ItemVariation
resolveItemName name
  | all isDigit name, not (null name) =
      case reads name of
        [(identifier, "")] -> Right (ItemVariation [identifier])
        _ -> Left (InvalidNumericItemId name)
  | otherwise = maybe (Left (UnknownItemName name)) Right (Map.lookup name routingItemVariations)

routingItemVariations :: Map.Map String ItemVariation
routingItemVariations = Map.fromList
  [ ("AIR_RUNE", variation [556, 4695, 4696, 4697])
  , ("ASTRAL_RUNE", variation [9075])
  , ("AXE", variation [1351, 1349, 1353, 1361, 1355, 1357, 1359, 6739, 23673, 23279, 13241, 20011])
  , ("BANANA", variation [1963])
  , ("BLOOD_RUNE", variation [565])
  , ("BROWN_APRON", variation [1757, 20208, 9780, 9781, 9782])
  , ("CAPESLOT", variation [4513, 4515])
  , ("CLIMBING_BOOTS", variation [3105, 23413])
  , ("COINS", variation [995])
  , ("CROSSBOW", variation [837, 767, 8880, 10156, 9174, 9177, 9179, 9181, 9183, 9185, 21902, 21012, 4734, 4938, 4937, 4936, 4935, 4934, 11785, 26374])
  , ("DUSTY_KEY", variation [1590])
  , ("EARTH_RUNE", variation [557, 4696, 4698, 4699])
  , ("ECTO_TOKEN", variation [4278])
  , ("FIRE_RUNE", variation [554, 4697, 4694, 4699])
  , ("GLARIALS_AMULET", variation [1718])
  , ("GLOWING_FUNGUS", variation [4075])
  , ("HEADSLOT", variation [4514, 4516])
  , ("LAW_RUNE", variation [563])
  , ("MACHETE", variation [975, 6313, 6315, 6317])
  , ("MAX_CAPE", variation [13280, 13342, 13329, 21186, 24134, 13331, 13333, 13335, 13337, 20760, 21285, 21284, 24133, 21776, 24232, 21780, 24233, 21784, 24234, 21898, 24135, 24855, 27363, 27365, 28902, 28906])
  , ("MAX_HOOD", variation [13281, 13330, 13332, 13334, 13336, 13338, 20764, 21282, 21778, 21782, 21786, 21900, 24857, 27366, 28904])
  , ("MAZE_KEY", variation [1542])
  , ("MIND_RUNE", variation [558])
  , ("MITH_GRAPPLE", variation [9418])
  , ("NATURE_RUNE", variation [561])
  , ("PICKAXE", variation [1265, 1267, 1269, 12297, 1273, 1271, 1275, 11920, 23680, 23276, 20014, 12797, 23677, 25376, 30351, 13243])
  , ("ROPE", variation [954])
  , ("SHANTAY_PASS", variation [1854])
  , ("SKAVID_MAP", variation [2376])
  , ("SOUL_RUNE", variation [566])
  , ("WATER_RUNE", variation [555, 4695, 4698, 4694])
  ]
 where
  variation = ItemVariation
