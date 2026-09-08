module ShortestPath.Requirements
  ( ItemExpr(..)
  , ItemTerm(..)
  , SkillReq(..)
  , GameVar(..)
  , VarKind(..)
  , VarbitId(..)
  , VarPlayerId(..)
  , VarOp(..)
  , VarReq(..)
  , parseItems
  , parseSkills
  , parseQuests
  , parseVars
  ) where

import Data.Char (isDigit, isSpace, toUpper)
import qualified Data.Text as T

import ShortestPath.GameVars

data ItemTerm = ItemTerm { itemName :: String, itemQuantity :: Int }
  deriving stock (Eq, Ord, Show)

data ItemExpr = ItemOne ItemTerm | ItemAnd [ItemExpr] | ItemOr [ItemExpr]
  deriving stock (Eq, Ord, Show)

data SkillReq = SkillReq { skillLevel :: Int, skillName :: String }
  deriving stock (Eq, Ord, Show)

data VarKind = Varbit | VarPlayer
  deriving stock (Eq, Ord, Show)

data GameVar = GameVarbit VarbitId | GameVarPlayer VarPlayerId
  deriving stock (Eq, Ord, Show)

data VarOp = VarEq | VarGt | VarLt | VarMask | VarCooldownMinutes
  deriving stock (Eq, Ord, Show)

data VarReq = VarReq { varRef :: GameVar, varValue :: Int, varOp :: VarOp }
  deriving stock (Eq, Ord, Show)

parseSkills :: String -> [SkillReq]
parseSkills = mapMaybe parseOne . splitChar ';'
 where
  parseOne raw =
    case words raw of
      level:name | all isDigit level -> Just (SkillReq (read level) (unwords name))
      _ -> Nothing

parseQuests :: String -> [String]
parseQuests = filter (not . null) . map trim . splitChar ';'

parseItems :: String -> Maybe ItemExpr
parseItems raw = parseOr (filter (not . isSpace) (map toUpper raw))
 where
  parseOr s =
    case splitText "|" s of
      [] -> Nothing
      [one] -> parseAnd one
      xs -> Just (ItemOr (mapMaybe parseAnd xs))
  parseAnd s =
    case splitText "&" s of
      [] -> Nothing
      [one] -> ItemOne <$> parseTerm one
      xs -> Just (ItemAnd (mapMaybe (fmap ItemOne . parseTerm) xs))
  parseTerm s =
    case break (== '=') s of
      (name, '=':qty) | not (null name), all isDigit qty -> Just (ItemTerm name (read qty))
      _ -> Nothing

parseVars :: VarKind -> String -> [VarReq]
parseVars kind = mapMaybe (parseVar kind) . filter (not . null) . splitChar ';'

parseVar :: VarKind -> String -> Maybe VarReq
parseVar kind raw = firstMatch [('=', VarEq), ('>', VarGt), ('<', VarLt), ('&', VarMask), ('@', VarCooldownMinutes)]
 where
  firstMatch [] = Nothing
  firstMatch ((c, op):rest) =
    case break (== c) raw of
      (a, _ : b) | all isDigit a, all isDigit b -> Just (VarReq (gameVar kind (read a)) (read b) op)
      _ -> firstMatch rest

gameVar :: VarKind -> Int -> GameVar
gameVar Varbit = GameVarbit . VarbitId
gameVar VarPlayer = GameVarPlayer . VarPlayerId

splitText :: String -> String -> [String]
splitText token = map T.unpack . T.splitOn (T.pack token) . T.pack

splitChar :: Char -> String -> [String]
splitChar c = splitText [c]

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []
