{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import qualified Data.Set as Set
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)

import ShortestPath.Account
import ShortestPath.AccountSemantics
import ShortestPath.BenchmarkProfiles
import qualified ShortestPath.GameVars.Varbits as VB
import qualified ShortestPath.GameVars.VarPlayers as VP
import ShortestPath.Pathfinder
import ShortestPath.Requirements (GameVar(..), VarReq(..), VarbitId(..), VarPlayerId(..))
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["validate", name] -> loadAccount name >>= validate
    ["compare", left, right] -> do
      before <- loadAccount left
      after <- loadAccount right
      compareProfiles before after
    ["coverage"] -> coverage
    ["vars"] -> variableAudit
    ["export-java", path] -> exportJava path
    ["check-java", path] -> checkJava path
    _ -> fail "usage: account-profile validate early|mid|end|maxed | compare BEFORE AFTER | coverage | vars | export-java PATH | check-java PATH"

exportJava :: FilePath -> IO ()
exportJava path = do
  value <- javaFixture
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (Aeson.encode value <> BL.singleton 10)

checkJava :: FilePath -> IO ()
checkJava path = do
  expected <- Aeson.encode <$> javaFixture
  actual <- BL.readFile path
  if actual == expected <> BL.singleton 10
    then putStrLn ("account fixture is deterministic and current: " <> path)
    else fail ("account fixture differs from regenerated output: " <> path)

javaFixture :: IO Aeson.Value
javaFixture = do
  world <- loadWorld defaultSourcePaths
  accounts <- mapM (\name -> do
    account <- maybe (fail ("unknown account profile: " <> name)) pure
      (benchmarkAccount name (allTransports world))
    pure (name, accountValue account)) benchmarkProfileNames
  pure $ Aeson.object
    [ "formatVersion" Aeson..= (1 :: Int)
    , "benchmarkNowMinutes" Aeson..= benchmarkNowMinutes
    , "profiles" Aeson..= Map.fromList accounts
    ]

accountValue :: AccountState -> Aeson.Value
accountValue account = Aeson.object
  [ "levels" Aeson..= accountLevels account
  , "completedQuests" Aeson..= Set.toAscList (accountCompletedQuests account)
  , "varbits" Aeson..= Map.fromList
      [(show identifier, value) | (VarbitId identifier, value) <- Map.toAscList (accountVarbits account)]
  , "varplayers" Aeson..= Map.fromList
      [(show identifier, value) | (VarPlayerId identifier, value) <- Map.toAscList (accountVarPlayers account)]
  , "inventory" Aeson..= accountInventory account
  , "equipment" Aeson..= accountEquipment account
  , "runePouch" Aeson..= accountRunePouch account
  , "bank" Aeson..= accountBank account
  , "diaries" Aeson..= Map.fromList
      [(show diary, show tier) | (diary, tier) <- Map.toAscList (accountDiaries account)]
  , "poh" Aeson..= pohValue (accountPoh account)
  , "plantedSpiritTrees" Aeson..= map plantedSpiritTreeCode (Set.toAscList (accountPlantedSpiritTrees account))
  , "fairyRingsUnlocked" Aeson..= accountFairyRingsUnlocked account
  , "runtime" Aeson..= runtimeValue (accountRuntime account)
  ]

pohValue :: PohBuild -> Aeson.Value
pohValue poh = Aeson.object
  [ "location" Aeson..= show (pohLocation poh)
  , "jewelleryBox" Aeson..= show (pohJewelleryBox poh)
  , "portals" Aeson..= portalValue (pohPortalDestinations poh)
  , "fairyRing" Aeson..= pohFairyRing poh
  , "spiritTree" Aeson..= pohSpiritTree poh
  , "obelisk" Aeson..= pohObelisk poh
  , "mountedGlory" Aeson..= pohMountedGlory poh
  , "mountedXerics" Aeson..= pohMountedXerics poh
  , "mountedDigsite" Aeson..= pohMountedDigsite poh
  , "mountedMythical" Aeson..= pohMountedMythical poh
  ]

plantedSpiritTreeCode :: PlantedSpiritTree -> String
plantedSpiritTreeCode FarmingGuildTree = "FARMING_GUILD"
plantedSpiritTreeCode PortSarimTree = "PORT_SARIM"
plantedSpiritTreeCode EtceteriaTree = "ETCETERIA"
plantedSpiritTreeCode BrimhavenTree = "BRIMHAVEN"
plantedSpiritTreeCode HosidiusTree = "HOSIDIUS"

portalValue :: PohPortalAccess -> Aeson.Value
portalValue AllPohPortals = Aeson.object
  [ "mode" Aeson..= ("all" :: String), "destinations" Aeson..= ([] :: [String]) ]
portalValue (SelectedPohPortals destinations) = Aeson.object
  [ "mode" Aeson..= ("selected" :: String)
  , "destinations" Aeson..= Set.toAscList destinations
  ]

runtimeValue :: RuntimeState -> Aeson.Value
runtimeValue runtime = Aeson.object
  [ "spellbook" Aeson..= show (runtimeSpellbook runtime)
  , "minigameTeleport" Aeson..= cooldownValue (runtimeMinigameTeleport runtime)
  , "arriveInsidePoh" Aeson..= runtimeArriveInsidePoh runtime
  ]

cooldownValue :: CooldownState -> Aeson.Value
cooldownValue CooldownReady = Aeson.object ["state" Aeson..= ("ready" :: String)]
cooldownValue (CooldownUsedAt minutes) = Aeson.object
  [ "state" Aeson..= ("usedAt" :: String), "minutes" Aeson..= minutes ]

loadAccount :: String -> IO AccountState
loadAccount name = do
  world <- loadWorld defaultSourcePaths
  maybe (fail ("unknown account profile: " <> name)) pure (benchmarkAccount name (allTransports world))

validate :: AccountState -> IO ()
validate account = do
  world <- loadWorld defaultSourcePaths
  let availability = prepareQueryTransports world profileQuery
      transports = allTransports world
      failures = foldr countFailure Map.empty [transportExplanation profileQuery False transport | transport <- transports]
  putStrLn ("local carried: " <> show (countLocals (carriedLocalTransports availability)))
  putStrLn ("local banked: " <> show (countLocals (bankedLocalTransports availability)))
  putStrLn ("global carried: " <> show (length (carriedGlobalTransports availability)))
  putStrLn ("global banked: " <> show (length (bankedGlobalTransports availability)))
  putStrLn ("POH: " <> show (accountPoh account))
  mapM_ putStrLn
    [ gatedCount profileQuery transports "quest" (not . null . quests)
    , gatedCount profileQuery transports "skill" (not . null . skills)
    , gatedCount profileQuery transports "item" (maybe False (const True) . items)
    , gatedCount profileQuery transports "var" (\transport -> not (null (varbits transport <> varPlayers transport)))
    ]
  mapM_ (putStrLn . renderFailure) (Map.toAscList failures)
  mapM_ (putStrLn . renderFamily availability) ["FAIRY_RING", "SPIRIT_TREE", "GNOME_GLIDER", "MAGIC_MUSHTREE", "QUETZAL", "HOT_AIR_BALLOON", "CANOE"]
 where
  profileQuery = (defaultQuery (packTile 0 0 0) (packTile 0 0 0)) { requirementMode = ConfiguredRequirements account, queryNowMinutes = benchmarkNowMinutes }

compareProfiles :: AccountState -> AccountState -> IO ()
compareProfiles before after = do
  world <- loadWorld defaultSourcePaths
  let query account = (defaultQuery (packTile 0 0 0) (packTile 0 0 0)) { requirementMode = ConfiguredRequirements account, queryNowMinutes = benchmarkNowMinutes }
      count account = countLocals (bankedLocalTransports (prepareQueryTransports world (query account))) + length (bankedGlobalTransports (prepareQueryTransports world (query account)))
  putStrLn ("newly available transports: " <> show (count after - count before))

coverage :: IO ()
coverage = do
  world <- loadWorld defaultSourcePaths
  let transports = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
      count predicate = length (filter predicate transports)
  mapM_ putStrLn
    [ "transports: " <> show (length transports)
    , "item requirements: " <> show (count (maybe False (const True) . items))
    , "skill requirements: " <> show (count (not . null . skills))
    , "quest requirements: " <> show (count (not . null . quests))
    , "varbits: " <> show (count (not . null . varbits))
    , "varplayers: " <> show (count (not . null . varPlayers))
    , "wilderness restrictions: " <> show (count (maybe False (const True) . maxWildernessLevel))
    , "consumable transports: " <> show (count consumable)
    , "generic parsed requirements understood: 100%"
    ]

variableAudit :: IO ()
variableAudit = do
  world <- loadWorld defaultSourcePaths
  let transports = allTransports world
      requirements = Map.fromListWith (<>)
        [ (varRef requirement, [(transportType transport, displayInfo transport, source transport)])
        | transport <- transports
        , requirement <- varbits transport <> varPlayers transport
        ]
      profiles = [(name, benchmarkAccount name transports) | name <- benchmarkProfileNames]
  mapM_ (printVariable profiles) (Map.toAscList requirements)

printVariable :: [(String, Maybe AccountState)] -> (GameVar, [(String, String, FilePath)]) -> IO ()
printVariable profiles (variable, uses) = do
  putStrLn (variableName variable <> " (" <> show variable <> ")")
  putStrLn ("  requirements: " <> intercalate ", " (unique [transport | (transport, _, _) <- uses]))
  putStrLn ("  uses: " <> show (length uses) <> " (" <> intercalate ", " (unique [file | (_, _, file) <- uses]) <> ")")
  mapM_ printProfile profiles
 where
  printProfile (name, Just account) = putStrLn ("  " <> name <> ": " <> maybe unmodelled show (gameVarValue account variable))
  printProfile (name, Nothing) = putStrLn ("  " <> name <> ": UNAVAILABLE")
  unmodelled = "UNMODELLED" <> maybe "" ((" (" <>) . (<> ")") . classificationName) (classifyUnmodelledVar variable)

classificationName :: UnmodelledVarClass -> String
classificationName RuntimeVar = "runtime state"
classificationName SpecialModeVar = "special mode"
classificationName NeedsInvestigation = "needs investigation"

gameVarValue :: AccountState -> GameVar -> Maybe Int
gameVarValue account variable = case variable of
  GameVarbit identifier -> Map.lookup identifier (accountVarbits account)
  GameVarPlayer identifier -> Map.lookup identifier (accountVarPlayers account)

variableName :: GameVar -> String
variableName variable = case variable of
  GameVarbit identifier -> maybe ("VARBIT_" <> show identifier) id (VB.varbitName identifier)
  GameVarPlayer identifier -> maybe ("VARPLAYER_" <> show identifier) id (VP.varPlayerName identifier)

unique :: Ord a => [a] -> [a]
unique = Map.keys . Map.fromList . map (\value -> (value, ()))

allTransports :: World -> [Transport]
allTransports world = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world

countLocals :: Map.Map Tile [Transport] -> Int
countLocals = sum . map length . Map.elems

countFailure :: TransportAvailability -> Map.Map String Int -> Map.Map String Int
countFailure availability counts =
  case availability of
    Available -> counts
    TransportTypeDisabled _ -> counts
    Unavailable failures -> foldr (\failure -> Map.insertWith (+) (failureName failure) 1) counts failures

failureName :: RequirementFailure -> String
failureName failure = case failure of
  MissingItems _ -> "items"
  MissingSkills _ -> "skills"
  MissingQuests _ -> "quests"
  FailedVarRequirements _ -> "vars"
  UnknownVarRequirements _ -> "unmodelled vars"
  MissingCapability _ -> "profile capability"

renderFailure :: (String, Int) -> String
renderFailure (name, count) = name <> " blocking: " <> show count

gatedCount :: Query -> [Transport] -> String -> (Transport -> Bool) -> String
gatedCount query transports name predicate =
  name <> "-gated available: " <> show (length [transport | transport <- transports, predicate transport, transportAvailable query True transport])


renderFamily :: QueryTransportAvailability -> String -> String
renderFamily availability family =
  family <> ": " <> if any ((== family) . transportType) (concat (Map.elems (bankedLocalTransports availability))) then "available" else "unavailable"
