module Quicksort where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (foldM)

quicksort :: Vector Int -> Vector Int
quicksort xs = V.create $ do
  v <- V.thaw xs
  quicksort' v 0 (MV.length v - 1)
  return v
    where
      quicksort' v l r
        | r - l <= 0 = return ()
        | otherwise = do
            pivot <- partition v l r
            quicksort' v l (pivot - 1)
            quicksort' v (pivot + 1) r
      partition v l r = do
        p <- MV.read v l
        let start = l + 1
        i <- foldM (f p) start [start .. r]
        MV.swap v l (i - 1)
        return (i - 1)
          where f p i j = do
                  aj <- MV.read v j
                  if (aj < p)
                    then MV.swap v j i >> return (i + 1)
                    else return i
