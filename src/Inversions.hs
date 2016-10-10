module Inversions (inversions, inversionsSeq, inversionsList) where

import Control.Arrow (first)
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (|>), ViewL((:<)))
import qualified Data.Sequence as S


-- List version
countSplitInvList :: (Ord a) => [a] -> [a] -> ([a], Int)
countSplitInvList = go ([], 0)
  where go (acc, n) [] r = (reverse acc ++ r, n)
        go (acc, n) l [] = (reverse acc ++ l, n)
        go (acc, n) l@(x:xs) r@(y:ys) =
          if x > y
          then go (y:acc, n + length l) l ys
          else go (x:acc, n) xs r

inversionsList :: (Ord a) => [a] -> ([a], Int)
inversionsList xs = if n <= 1 then (xs, 0) else (d, x + y + z)
  where n = length xs
        pivot = n `div` 2
        (l, r) = splitAt pivot xs
        (b, x) = inversionsList l
        (c, y) = inversionsList r
        (d, z) = countSplitInvList b c

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
          (S.EmptyL, _) -> (acc <> r, n)
          (_, S.EmptyL) -> (acc <> l, n)
          (x :< xs, y :< ys) -> if x > y
                                then go (acc |> y, n + S.length l) l ys
                                else go (acc |> x, n) xs r

inversions :: (Ord a) => [a] -> ([a], Int)
inversions = first toList . inversionsSeq . S.fromList
