module ShortestPath.Internal.MutableHeap
  ( MutableHeap
  , heapNew
  , heapPop
  , heapPush
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as Mutable

data HeapStorage s = HeapStorage
  { heapPriorities :: Mutable.MVector s Int
  , heapStates :: Mutable.MVector s Int
  , heapCosts :: Mutable.MVector s Int
  }

data MutableHeap s = MutableHeap
  { heapStorageRef :: STRef s (HeapStorage s)
  , heapSizeRef :: STRef s Int
  }

heapNew :: Int -> ST s (MutableHeap s)
{-# INLINE heapNew #-}
heapNew requestedCapacity = do
  let capacity = max 1 requestedCapacity
  priorities <- Mutable.new capacity
  states <- Mutable.new capacity
  costs <- Mutable.new capacity
  storage <- newSTRef (HeapStorage priorities states costs)
  size <- newSTRef 0
  pure (MutableHeap storage size)

heapPush :: MutableHeap s -> Int -> Int -> Int -> ST s ()
{-# INLINE heapPush #-}
heapPush heap priority state cost = do
  size <- readSTRef (heapSizeRef heap)
  storage <- ensureCapacity heap size
  writeEntry storage size (priority, state, cost)
  writeSTRef (heapSizeRef heap) (size + 1)
  bubbleUp storage size

heapPop :: MutableHeap s -> ST s (Maybe (Int, Int, Int))
{-# INLINE heapPop #-}
heapPop heap = do
  size <- readSTRef (heapSizeRef heap)
  if size == 0
    then pure Nothing
    else do
      storage <- readSTRef (heapStorageRef heap)
      entry <- heapEntry storage 0
      let lastIx = size - 1
      writeSTRef (heapSizeRef heap) lastIx
      when (lastIx > 0) $ do
        moveEntry storage lastIx 0
        bubbleDown storage lastIx 0
      pure (Just entry)

ensureCapacity :: MutableHeap s -> Int -> ST s (HeapStorage s)
{-# INLINE ensureCapacity #-}
ensureCapacity heap size = do
  storage <- readSTRef (heapStorageRef heap)
  if size < Mutable.length (heapPriorities storage)
    then pure storage
    else do
      let extra = Mutable.length (heapPriorities storage)
      priorities <- Mutable.grow (heapPriorities storage) extra
      states <- Mutable.grow (heapStates storage) extra
      costs <- Mutable.grow (heapCosts storage) extra
      let grown = HeapStorage priorities states costs
      writeSTRef (heapStorageRef heap) grown
      pure grown

bubbleUp :: HeapStorage s -> Int -> ST s ()
bubbleUp storage ix
  | ix <= 0 = pure ()
  | otherwise = do
      let parent = (ix - 1) `div` 2
      childEntry <- heapEntry storage ix
      parentEntry <- heapEntry storage parent
      if entryLess childEntry parentEntry
        then swapEntries storage ix parent >> bubbleUp storage parent
        else pure ()

bubbleDown :: HeapStorage s -> Int -> Int -> ST s ()
bubbleDown storage size ix = do
  let left = ix * 2 + 1
      right = left + 1
  if left >= size
    then pure ()
    else do
      smallest <- do
        leftEntry <- heapEntry storage left
        if right >= size
          then pure left
          else do
            rightEntry <- heapEntry storage right
            pure (if entryLess rightEntry leftEntry then right else left)
      here <- heapEntry storage ix
      child <- heapEntry storage smallest
      if entryLess child here
        then swapEntries storage ix smallest >> bubbleDown storage size smallest
        else pure ()

heapEntry :: HeapStorage s -> Int -> ST s (Int, Int, Int)
{-# INLINE heapEntry #-}
heapEntry storage ix =
  (,,) <$> Mutable.read (heapPriorities storage) ix <*> Mutable.read (heapStates storage) ix <*> Mutable.read (heapCosts storage) ix

entryLess :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
{-# INLINE entryLess #-}
entryLess (leftPriority, leftState, leftCost) (rightPriority, rightState, rightCost) =
  (leftPriority, leftCost, leftState) < (rightPriority, rightCost, rightState)

swapEntries :: HeapStorage s -> Int -> Int -> ST s ()
{-# INLINE swapEntries #-}
swapEntries storage left right = do
  entry <- heapEntry storage left
  moveEntry storage right left
  writeEntry storage right entry

moveEntry :: HeapStorage s -> Int -> Int -> ST s ()
{-# INLINE moveEntry #-}
moveEntry storage from to = heapEntry storage from >>= writeEntry storage to

writeEntry :: HeapStorage s -> Int -> (Int, Int, Int) -> ST s ()
{-# INLINE writeEntry #-}
writeEntry storage ix (priority, state, cost) = do
  Mutable.write (heapPriorities storage) ix priority
  Mutable.write (heapStates storage) ix state
  Mutable.write (heapCosts storage) ix cost
