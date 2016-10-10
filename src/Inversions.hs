{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Inversions ( inversions
                  , inversionsSeq
                  , inversionsList
                  , inversionsListAndLength
                  , inversionsGeneric) where

import Control.Arrow (first)
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>), ViewL((:<)))
import qualified Data.Sequence as S

-- consistent wrapper func for tests

inversions :: (Ord a) => [a] -> ([a], Int)
-- inversions = inversionsGeneric
-- List version
-- inversions = inversionsList
-- List and length version
inversions xs = inversionsListAndLength (xs, length xs)
-- Seq version
-- inversions = first toList . inversionsSeq . S.fromList

-- List version
inversionsList :: (Ord a) => [a] -> ([a], Int)
inversionsList xs = if n <= 1 then (xs, 0) else (d, x + y + z)
  where n = length xs
        pivot = n `div` 2
        (l, r) = splitAt pivot xs
        (b, x) = inversionsList l
        (c, y) = inversionsList r
        (d, z) = countSplitInvList b c

countSplitInvList :: (Ord a) => [a] -> [a] -> ([a], Int)
countSplitInvList = go ([], 0)
  where go (acc, n) [] r = ({-# SCC append_list #-} ({-# SCC reverse_list #-} reverse acc) ++ r, n)
        go (acc, n) l [] = ({-# SCC append_list #-} ({-# SCC reverse_list #-} reverse acc) ++ l, n)
        go (acc, n) l@(x:xs) r@(y:ys) =
          if x > y
          then go (y:acc, n + {-# SCC length_list #-} length l) l ys
          else go (x:acc, n) xs r

-- List version (pass length)
inversionsListAndLength :: (Ord a) => ([a], Int) -> ([a], Int)
inversionsListAndLength (xs, n) = if n <= 1 then (xs, 0) else (d, x + y + z)
  where pivot = n `div` 2
        right_length = n - pivot
        (l, r) = splitAt pivot xs
        (b, x) = inversionsListAndLength (l, pivot)
        (c, y) = inversionsListAndLength (r, right_length)
        (d, z) = countSplitInvListAndLength (b, pivot) (c, right_length)

countSplitInvListAndLength :: (Ord a) => ([a], Int) -> ([a], Int) -> ([a], Int)
countSplitInvListAndLength = go ([], 0)
  where go (acc, n) ([], _) (r, _) = (reverse acc ++ r, n)
        go (acc, n) (l, _) ([], _) = (reverse acc ++ l, n)
        go (acc, n) (l@(x:xs), ln) (r@(y:ys), rn) =
          if x > y
          then go (y:acc, n + ln) (l, ln) (ys, rn - 1)
          else go (x:acc, n) (xs, ln - 1) (r, rn)

-- Sequence version

inversionsSeq :: (Ord a) => Seq a -> (Seq a, Int)
inversionsSeq xs = if n <= 1 then (xs, 0) else (d, x + y + z)
  where n = S.length xs
        pivot = n `div` 2
        (l, r) = S.splitAt pivot xs
        (b, x) = inversionsSeq l
        (c, y) = inversionsSeq r
        (d, z) = countSplitInvSeq b c

countSplitInvSeq :: (Ord a) => Seq a -> Seq a -> (Seq a, Int)
countSplitInvSeq = go (S.empty, 0)
  where go (acc, n) l r = case (S.viewl l, S.viewl r) of
          (S.EmptyL, _) -> ({-# SCC append_seq #-} acc <> r, n)
          (_, S.EmptyL) -> ({-# SCC append_seq #-} acc <> l, n)
          (x :< xs, y :< ys) -> if x > y
                                then go (acc |> y, n + {-# SCC length_seq #-} S.length l) l ys
                                else go (acc |> x, n) xs r

-- typeclass approach

-- | A typeclass for containers that support the operations needed for mergesort.
class Monoid (t a) => MergeSortable t a where
  isEmpty :: t a -> Bool
  getLength :: t a -> Int
  mySplitAt :: Int -> t a -> (t a, t a)
  snoc :: t a -> a -> t a
  uncons :: t a -> Maybe (a, t a)
  reverseIfNeeded :: t a -> t a -- for types where snoc is inefficient, just reverse at end

instance MergeSortable [] a where
  isEmpty = null
  getLength = length
  mySplitAt = splitAt
  snoc = flip (:)
  uncons [] = Nothing
  uncons (x:xs) = Just (x, xs)
  reverseIfNeeded = reverse

instance MergeSortable Seq a where
  isEmpty = S.null
  getLength = S.length
  mySplitAt = S.splitAt
  snoc = (|>)
  uncons xs = case S.viewl xs of
    S.EmptyL -> Nothing
    x :< xs' -> Just (x, xs')
  reverseIfNeeded = id

-- Now, this function can take kind of container for which a MergeSortable instance exists
inversionsGeneric :: (MergeSortable t a, Ord a) => t a -> (t a, Int)
inversionsGeneric xs = if n <= 1 then (xs, 0) else (d, x + y + z)
  where n = getLength xs
        pivot = n `div` 2
        (l, r) = mySplitAt pivot xs
        (b, x) = inversionsGeneric l
        (c, y) = inversionsGeneric r
        (d, z) = countSplitInvGeneric b c

countSplitInvGeneric :: (MergeSortable t a, Ord a) => t a -> t a -> (t a, Int)
countSplitInvGeneric = go (mempty, 0)
  where go (acc, n) l r = case (uncons l, uncons r) of
          (Nothing, _) -> (reverseIfNeeded acc `mappend` r, n)
          (_, Nothing) -> (reverseIfNeeded acc `mappend` l, n)
          (Just (x, xs), Just (y, ys)) ->
            if x > y
            then go (acc `snoc` y, n + getLength l) l ys
            else go (acc `snoc` x, n) xs r

