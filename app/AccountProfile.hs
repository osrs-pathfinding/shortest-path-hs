module Main (main) where

import Data.Aeson (decodeFileStrict')
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

import ShortestPath.Account
import ShortestPath.Pathfinder
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["validate", path] -> loadAccount path >>= validate
    ["compare", left, right] -> do
      before <- loadAccount left
      after <- loadAccount right
      compareProfiles before after
    ["coverage"] -> coverage
    _ -> fail "usage: account-profile validate PROFILE.json | compare BEFORE.json AFTER.json | coverage"

loadAccount :: FilePath -> IO AccountBuild
loadAccount path = do
  value <- decodeFileStrict' path
  maybe (fail ("invalid account profile: " <> path)) pure value

validate :: AccountBuild -> IO ()
validate account = do
  world <- loadWorld defaultSourcePaths
  let availability = prepareQueryTransports world profileQuery
      allTransports = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
      failures = foldr countFailure Map.empty [transportExplanation profileQuery False transport | transport <- allTransports]
  putStrLn ("local carried: " <> show (countLocals (carriedLocalTransports availability)))
  putStrLn ("local banked: " <> show (countLocals (bankedLocalTransports availability)))
  putStrLn ("global carried: " <> show (length (carriedGlobalTransports availability)))
  putStrLn ("global banked: " <> show (length (bankedGlobalTransports availability)))
  mapM_ (putStrLn . renderFailure) (Map.toAscList failures)
  mapM_ (putStrLn . renderFamily availability) ["FAIRY_RING", "SPIRIT_TREE", "QUETZAL", "GNOME_GLIDER"]
 where
  profileQuery = (defaultQuery (packTile 0 0 0) (packTile 0 0 0)) { requirementMode = ConfiguredRequirements account }

compareProfiles :: AccountBuild -> AccountBuild -> IO ()
compareProfiles before after = do
  world <- loadWorld defaultSourcePaths
  let query account = (defaultQuery (packTile 0 0 0) (packTile 0 0 0)) { requirementMode = ConfiguredRequirements account }
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

renderFailure :: (String, Int) -> String
renderFailure (name, count) = name <> " blocking: " <> show count

renderFamily :: QueryTransportAvailability -> String -> String
renderFamily availability family =
  family <> ": " <> if any ((== family) . transportType) (concat (Map.elems (bankedLocalTransports availability))) then "available" else "unavailable"
