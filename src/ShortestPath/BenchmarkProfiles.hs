module ShortestPath.BenchmarkProfiles
  ( BenchmarkProfile(..)
  , Progression(..)
  , ItemLoadout(..)
  , benchmarkProfileNames
  , benchmarkAccount
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Bits ((.|.))

import ShortestPath.Account
import ShortestPath.Requirements
import ShortestPath.Transport

data Progression = Progression
  { progressionLevels :: ItemCounts
  , progressionQuests :: Set.Set String
  , progressionDiaries :: Map.Map String DiaryTier
  , progressionFairyRings :: Bool
  , progressionQuetzalPlatforms :: Set.Set Int
  }
  deriving stock (Eq, Show)

data ItemLoadout = ItemLoadout
  { loadoutInventory :: ItemCounts
  , loadoutEquipment :: ItemCounts
  , loadoutRunePouch :: ItemCounts
  }
  deriving stock (Eq, Show)

data BenchmarkProfile = BenchmarkProfile
  { profileName :: String
  , profileProgression :: Progression
  , profilePoh :: PohBuild
  , profileCarried :: ItemLoadout
  , profileBank :: ItemCounts
  , profileRuntime :: RuntimeState
  }
  deriving stock (Eq, Show)

benchmarkProfileNames :: [String]
benchmarkProfileNames = ["early", "mid", "end", "maxed"]

benchmarkAccount :: String -> [Transport] -> Maybe AccountBuild
benchmarkAccount name transports = compile <$> profile
 where
  profile = case name of
    "early" -> Just earlyProfile
    "mid" -> Just (midProfile allQuests)
    "end" -> Just (endProfile allQuests)
    "maxed" -> Just (maxedProfile allQuests allItems)
    _ -> Nothing
  allQuests = Set.fromList (concatMap quests transports)
  allItems = Map.fromList [(item, 1000) | transport <- transports, item <- itemNames =<< maybeToList (items transport)]
  compile value =
    emptyAccountBuild
      { accountLevels = progressionLevels progress
      , accountCompletedQuests = progressionQuests progress
      , accountVarbits = diaryVarbits (progressionDiaries progress) <> platformVarbits (progressionQuetzalPlatforms progress) <> Map.fromList [(4070, 0)]
      , accountVarPlayers = Map.fromList [(4560, 0), (888, 0), (892, 0), (4182, platformMask (progressionQuetzalPlatforms progress))]
      , accountInventory = loadoutInventory loadout
      , accountEquipment = loadoutEquipment loadout
      , accountRunePouch = loadoutRunePouch loadout
      , accountBank = profileBank value
      , accountDiaries = progressionDiaries progress
      , accountPoh = profilePoh value
      , accountFairyRingsUnlocked = progressionFairyRings progress
      , accountQuetzalPlatforms = progressionQuetzalPlatforms progress
      , accountRuntime = profileRuntime value
      }
   where
    progress = profileProgression value
    loadout = profileCarried value

earlyProfile :: BenchmarkProfile
earlyProfile = BenchmarkProfile "early" (progression earlyLevels earlyQuests (allDiaries Medium) True (Set.singleton 1)) basicPoh earlyLoadout earlyBank standardRuntime

midProfile :: Set.Set String -> BenchmarkProfile
midProfile allQuests = BenchmarkProfile "mid" (progression midLevels allQuests (allDiaries Hard) True allPlatforms) midPoh midLoadout midBank standardRuntime

endProfile :: Set.Set String -> BenchmarkProfile
endProfile allQuests = BenchmarkProfile "end" (progression (Map.insert "Quest" 327 endLevels) allQuests endDiaries True allPlatforms) maxedPoh endLoadout endBank standardRuntime

maxedProfile :: Set.Set String -> ItemCounts -> BenchmarkProfile
maxedProfile allQuests allItems = BenchmarkProfile "maxed" (progression (Map.fromList [(skill, 99) | skill <- allSkills] <> Map.fromList [("Quest", 327), ("Total", 2376)]) allQuests (allDiaries Elite) True allPlatforms) maxedPoh maxedLoadout (allItems <> endBank) standardRuntime

progression :: ItemCounts -> Set.Set String -> Map.Map String DiaryTier -> Bool -> Set.Set Int -> Progression
progression = Progression

allDiaries :: DiaryTier -> Map.Map String DiaryTier
allDiaries tier = Map.fromList [(name, tier) | name <- diaryNames]

endDiaries :: Map.Map String DiaryTier
endDiaries = Map.insert "Lumbridge & Draynor" Elite (allDiaries Hard)

diaryNames :: [String]
diaryNames = ["Ardougne", "Desert", "Falador", "Fremennik", "Kandarin", "Karamja", "Kourend & Kebos", "Lumbridge & Draynor", "Morytania", "Varrock", "Western Provinces", "Wilderness"]

diaryVarbits :: Map.Map String DiaryTier -> Map.Map Int Int
diaryVarbits diaries
  | all (>= Elite) (Map.elems diaries) = Map.fromList [(key, 1) | key <- [4461, 4465, 4469, 4474, 4478, 4482, 4486, 4490, 4494, 4498, 4566]]
  | Map.findWithDefault NoDiary "Lumbridge & Draynor" diaries >= Elite = Map.singleton 4566 1
  | otherwise = Map.empty

allPlatforms :: Set.Set Int
allPlatforms = Set.fromList [1 .. 8]

platformVarbits :: Set.Set Int -> Map.Map Int Int
platformVarbits platforms = Map.fromList [(29271, 1), (29273, 1), (29275, 1), (33120, 1)] <> if Set.null platforms then Map.empty else Map.singleton 4182 (platformMask platforms)

platformMask :: Set.Set Int -> Int
platformMask = Set.foldr (.|.) 0 . Set.map bitFor
 where
  bitFor platform = 2 ^ platform

itemNames :: ItemExpr -> [String]
itemNames expression = case expression of
  ItemOne term -> [itemName term]
  ItemAnd terms -> concatMap itemNames terms
  ItemOr terms -> concatMap itemNames terms

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] pure

allSkills :: [String]
allSkills = ["Attack", "Strength", "Defence", "Hitpoints", "Ranged", "Prayer", "Magic", "Agility", "Herblore", "Thieving", "Crafting", "Fletching", "Slayer", "Hunter", "Mining", "Smithing", "Fishing", "Cooking", "Firemaking", "Woodcutting", "Farming", "Runecraft", "Construction", "Sailing"]

levels :: [Int] -> ItemCounts
levels values = Map.fromList (zip allSkills values)

earlyLevels, midLevels, endLevels :: ItemCounts
earlyLevels = levels [70, 75, 70, 75, 70, 60, 70, 70, 65, 65, 65, 65, 65, 65, 65, 65, 65, 70, 70, 65, 65, 60, 60, 60]
midLevels = levels [80, 85, 80, 85, 80, 70, 85, 80, 78, 82, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 83, 75, 78, 75]
endLevels = levels [90, 95, 90, 95, 90, 85, 94, 90, 90, 91, 90, 90, 95, 90, 85, 91, 90, 95, 90, 90, 91, 85, 85, 85]

earlyQuests :: Set.Set String
earlyQuests = Set.fromList ["Another Slice of H.A.M.", "Biohazard", "Bone Voyage", "Children of the Sun", "Client of Kourend", "Creature of Fenkenstrain", "Death to the Dorgeshuun", "Enter the Abyss", "Garden of Tranquillity", "Haunted Mine", "Holy Grail", "In Search of the Myreque", "Lost City", "Monkey Madness I", "Nature Spirit", "Observatory Quest", "Plague City", "Priest in Peril", "Regicide", "Sea Slug", "Shades of Mort'ton", "Tai Bwo Wannai Trio", "The Corsair Curse", "The Fremennik Trials", "The Giant Dwarf", "The Grand Tree", "The Lost Tribe", "Tree Gnome Village", "Twilight's Promise", "Watchtower"]

basicPoh, midPoh, maxedPoh :: PohBuild
basicPoh = PohBuild "Rimmington" NoJewelleryBox Set.empty False False False False False False False
midPoh = PohBuild "Rimmington" FancyJewelleryBox (Set.fromList ["Varrock Portal", "Falador Portal", "Camelot Portal", "Ardougne Portal", "Kourend Portal", "Barrows Portal"]) False False False True True True True
maxedPoh = PohBuild "Rimmington" OrnateJewelleryBox (Set.singleton "*") True True True True True True True

standardRuntime :: RuntimeState
standardRuntime = RuntimeState "Standard" True True

earlyLoadout, midLoadout, endLoadout, maxedLoadout :: ItemLoadout
earlyLoadout = ItemLoadout (Map.fromList [("772", 1), ("2552", 1), ("3853", 1), ("1704", 1), ("8013", 1), ("995", 100000)]) Map.empty standardRunes
midLoadout = ItemLoadout (Map.fromList [("772", 1), ("2552", 1), ("1704", 1), ("3853", 1), ("11866", 1), ("11190", 1), ("8013", 1), ("29271", 1), ("995", 1000000)]) Map.empty standardRunes
endLoadout = ItemLoadout (Map.fromList [("9813", 1), ("2552", 1), ("1704", 1), ("8013", 1), ("995", 5000000)]) Map.empty standardRunes
maxedLoadout = ItemLoadout (Map.fromList [("13280", 1), ("13069", 1), ("8013", 1), ("995", 10000000)]) Map.empty standardRunes

standardRunes :: ItemCounts
standardRunes = Map.fromList [("554", 10000), ("555", 10000), ("556", 10000), ("563", 10000)]

earlyBank :: ItemCounts
earlyBank = itemBank ["772", "2552", "3853", "1704", "11118", "11105", "21146", "11980", "8013", "995"]

midBank :: ItemCounts
midBank = itemBank
  [ -- Charged jewellery.
    "2552", "3853", "11978", "11968", "11972", "11194", "11866", "11980", "21146", "21166"
  , -- Standard spellbook tablets and commonly stocked teleport scrolls.
    "8007", "8008", "8009", "8010", "8011", "8012", "8013"
  , "12402", "12403", "12404", "12406", "12407", "12409", "12410", "12938"
  , -- Reusable quest and travel unlocks.
    "772", "4251", "6707", "13393", "13660", "19564", "21389", "21760", "22400"
  , "22599", "22601", "23946", "25818", "29273", "29893", "32399"
  , -- Hard diary equipment with useful travel actions.
    "11140", "13110", "13114", "13123", "13127", "13131", "13135", "13139", "13143", "22945"
  , -- Common rune and overland-transport requirements.
    "AIR_RUNE", "WATER_RUNE", "EARTH_RUNE", "FIRE_RUNE", "LAW_RUNE", "NATURE_RUNE"
  , "COINS", "AXE", "PICKAXE", "ROPE", "MACHETE", "SHANTAY_PASS", "CROSSBOW", "MITH_GRAPPLE"
  ]

endBank :: ItemCounts
endBank = itemBank
  [ -- Mid-game staples retained at their best common charge.
    "2552", "3853", "11978", "11968", "11972", "11194", "11866", "11980", "21146", "21166"
  , "8007", "8008", "8009", "8010", "8011", "8012", "8013"
  , "12402", "12403", "12404", "12405", "12406", "12407", "12408", "12409", "12410", "12411", "12642", "12938"
  , -- Late-game reusable and earned convenience teleports.
    "772", "4251", "6707", "13393", "13660", "19564", "21268", "21389", "21760", "22400"
  , "22599", "22601", "23458", "23946", "25818", "26818", "26948", "28327", "29275", "29893", "32399", "33104"
  , -- Elite diary equipment.
    "13103", "13111", "13115", "13124", "13128", "13132", "13136", "13140", "13144", "22947"
  , "AIR_RUNE", "WATER_RUNE", "EARTH_RUNE", "FIRE_RUNE", "LAW_RUNE", "NATURE_RUNE"
  , "COINS", "AXE", "PICKAXE", "ROPE", "MACHETE", "SHANTAY_PASS", "CROSSBOW", "MITH_GRAPPLE"
  ]

itemBank :: [String] -> ItemCounts
itemBank items = Map.fromList [(item, 1000) | item <- items]
