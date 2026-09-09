{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import System.Exit (die)

import ShortestPath.Account
import ShortestPath.BenchmarkProfiles
import qualified ShortestPath.GameVars.Varbits as VB
import qualified ShortestPath.GameVars.VarPlayers as VP
import ShortestPath.Pathfinder
import ShortestPath.Requirements
import ShortestPath.Tile
import ShortestPath.Transport
import ShortestPath.World

data Request = Request { requestId :: Int, requestRouteId :: String, requestRouteName :: String, requestProfile :: String }
instance FromJSON Request where
  parseJSON = withObject "request" $ \o -> Request <$> o .: "id" <*> o .: "routeId" <*> o .:? "routeName" .!= "" <*> o .: "profile"

data Step = Step { stepCoordinate :: String, stepKind :: String, stepLabel :: Maybe String }
instance FromJSON Step where
  parseJSON = withObject "step" $ \o -> Step <$> o .: "coordinate" <*> o .: "kind" <*> o .:? "label"

data Response = Response { responseId :: Int, responsePath :: [Step] }
instance FromJSON Response where
  parseJSON = withObject "response" $ \o -> Response <$> o .: "id" <*> o .:? "path" .!= []

data Failed = Failed { failedRouteId :: String, failedProfile :: String }
instance FromJSON Failed where
  parseJSON = withObject "oracle row" $ \o -> Failed <$> o .: "routeId" <*> o .: "accountProfile"

main :: IO ()
main = do
  [requestFile, responseFile, oracleFile] <- getArgsOrDie
  world <- loadWorld defaultSourcePaths
  let transports = concat (Map.elems (worldTransports world)) <> worldGlobalTeleports world
      accounts = Map.fromList [(name, account) | name <- benchmarkProfileNames, Just account <- [benchmarkAccount name transports]]
  requests <- readJsonLines requestFile
  responses <- readJsonLines responseFile
  failures <- readJsonLines oracleFile
  let requestMap = Map.fromList [(requestId r, r) | r <- requests]
      failedByRoute = Map.fromListWith (<>) [(failedRouteId f, [failedProfile f]) | f <- failures]
  putStrLn "routeId\trouteName\tsuccessProfile\tfailedProfiles\trequirementFailures"
  let chosen = Map.fromListWith prefer
        [ (requestRouteId request, (request, response))
        | response <- responses
        , Just request <- [Map.lookup (responseId response) requestMap]
        ]
  mapM_ (uncurry (printRoute failedByRoute accounts transports)) (Map.elems chosen)

getArgsOrDie :: IO [String]
getArgsOrDie = do
  args <- getArgs
  if length args == 3 then pure args else die "usage: unreachable-diagnostic REQUESTS RESPONSES UNREACHABLE_ORACLE"

readJsonLines :: FromJSON a => FilePath -> IO [a]
readJsonLines path = do
  lines' <- LBS.lines <$> LBS.readFile path
  pure (mapMaybe decode lines')

prefer :: (Request, Response) -> (Request, Response) -> (Request, Response)
prefer left@(_, leftResponse) right
  | null (responsePath leftResponse) = right
  | otherwise = left

printRoute :: Map.Map String [String] -> Map.Map String AccountBuild -> [Transport] -> Request -> Response -> IO ()
printRoute failedByRoute accounts transports request response = do
  let failedProfiles = Map.findWithDefault [] (requestRouteId request) failedByRoute
      failures = nub (concatMap (profileFailures failedProfiles) (responsePath response))
  putStrLn (intercalate "\t" [requestRouteId request, requestRouteName request, requestProfile request, intercalate "," failedProfiles, if null failures then "-" else intercalate "; " failures])
 where
  profileFailures failedProfiles step = case (stepKind step, parseCoordinate (stepCoordinate step)) of
    ("transport", Just destination) ->
      [ profile <> ":" <> failure
      | profile <- failedProfiles
      , Just account <- [Map.lookup profile accounts]
      , transport <- matchingTransports destination (stepLabel step)
      , failure <- failuresFor account transport
      ]
    _ -> []
  matchingTransports destination label = filter matches transports
   where
    matches transport = destinationOf transport == Just destination && maybe True (== displayInfo transport) label
  failuresFor account transport =
    if Available `elem` availability then [] else [renderFailure failure | Unavailable failures <- availability, failure <- failures]
   where
    availability = [transportExplanation query banked transport | banked <- [False, True], let query = (defaultQuery (packTile 0 0 0) (packTile 0 0 0)) { requirementMode = ConfiguredRequirements account }]

renderFailure :: RequirementFailure -> String
renderFailure failure = case failure of
  MissingItems items -> "MissingItems " <> renderItems items
  MissingSkills skills -> "MissingSkills " <> intercalate "," [show (skillLevel skill) <> " " <> skillName skill | skill <- skills]
  MissingQuests quests -> "MissingQuests " <> intercalate "," quests
  FailedVarRequirements vars -> "FailedVarRequirements " <> intercalate "," (map renderVar vars)
  UnknownVarRequirements vars -> "UnknownVarRequirements " <> intercalate "," (map renderVar vars)
  MissingCapability capability -> "MissingCapability " <> capability
 where
  renderItems items = case items of
    ItemOne term -> itemName term <> "x" <> show (itemQuantity term)
    ItemAnd terms -> intercalate "&" (map renderItems terms)
    ItemOr terms -> intercalate "|" (map renderItems terms)
  renderVar requirement = variableName (varRef requirement) <> renderOp (varOp requirement) <> show (varValue requirement)
  renderOp op = case op of
    VarEq -> "="
    VarGt -> ">"
    VarLt -> "<"
    VarMask -> "&"
    VarCooldownMinutes -> "@"
  variableName variable = case variable of
    GameVarbit identifier -> maybe ("VARBIT_" <> show identifier) id (VB.varbitName identifier)
    GameVarPlayer identifier -> maybe ("VARPLAYER_" <> show identifier) id (VP.varPlayerName identifier)

destinationOf :: Transport -> Maybe Tile
destinationOf = destination

parseCoordinate :: String -> Maybe Tile
parseCoordinate raw = case map read (split '/' raw) of
  [x, y, plane] -> Just (packTile x y plane)
  _ -> Nothing
 where
  split separator value = case break (== separator) value of
    (head', []) -> [head']
    (head', _:tail') -> head' : split separator tail'
