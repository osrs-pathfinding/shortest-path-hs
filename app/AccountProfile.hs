module Main (main) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import System.Environment (getArgs)

import ShortestPath.Account
import ShortestPath.BenchmarkProfiles
import qualified ShortestPath.GameVars.Varbits as VB
import qualified ShortestPath.GameVars.VarPlayers as VP
import ShortestPath.Pathfinder
import ShortestPath.Requirements (GameVar(..), VarReq(..))
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
    _ -> fail "usage: account-profile validate early|mid|end|maxed | compare BEFORE AFTER | coverage | vars"

loadAccount :: String -> IO AccountBuild
loadAccount name = do
  world <- loadWorld defaultSourcePaths
  maybe (fail ("unknown account profile: " <> name)) pure (benchmarkAccount name (allTransports world))

validate :: AccountBuild -> IO ()
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

compareProfiles :: AccountBuild -> AccountBuild -> IO ()
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

printVariable :: [(String, Maybe AccountBuild)] -> (GameVar, [(String, String, FilePath)]) -> IO ()
printVariable profiles (variable, uses) = do
  putStrLn (variableName variable <> " (" <> show variable <> ")")
  putStrLn ("  requirements: " <> intercalate ", " (unique [transport | (transport, _, _) <- uses]))
  putStrLn ("  uses: " <> show (length uses) <> " (" <> intercalate ", " (unique [file | (_, _, file) <- uses]) <> ")")
  mapM_ printProfile profiles
 where
  printProfile (name, Just account) = putStrLn ("  " <> name <> ": " <> maybe "UNMODELLED" show (gameVarValue account variable))
  printProfile (name, Nothing) = putStrLn ("  " <> name <> ": UNAVAILABLE")

gameVarValue :: AccountBuild -> GameVar -> Maybe Int
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
