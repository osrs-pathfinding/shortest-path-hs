module ShortestPath.GameVars
  ( VarbitId(..)
  , VarPlayerId(..)
  ) where

newtype VarbitId = VarbitId Int
  deriving stock (Eq, Ord, Show)

newtype VarPlayerId = VarPlayerId Int
  deriving stock (Eq, Ord, Show)
