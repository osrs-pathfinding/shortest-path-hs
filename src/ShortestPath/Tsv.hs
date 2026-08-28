module ShortestPath.Tsv
  ( Row
  , readRows
  , parseRows
  , field
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type Row = Map.Map String String

readRows :: FilePath -> IO [Row]
readRows path = parseRows <$> readFile path

parseRows :: String -> [Row]
parseRows "" = []
parseRows text =
  case lines text of
    [] -> []
    headerLine:body ->
      let headers = splitTabs (stripHeader headerLine)
       in map (row headers) (filter dataLine body)

field :: String -> Row -> String
field = Map.findWithDefault ""

stripHeader :: String -> String
stripHeader ('#':' ':xs) = xs
stripHeader ('#':xs) = xs
stripHeader xs = xs

dataLine :: String -> Bool
dataLine "" = False
dataLine ('#':_) = False
dataLine xs = any (/= '\t') xs

row :: [String] -> String -> Row
row headers line = Map.fromList (zip headers (splitTabs line <> repeat ""))

splitTabs :: String -> [String]
splitTabs = map T.unpack . T.splitOn (T.pack "\t") . T.pack
