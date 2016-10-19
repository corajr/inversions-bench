module Quicksort where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.STRef
import Control.Monad (forM_, when)

quicksort :: Vector Int -> Vector Int
quicksort xs = V.create $ do
  v <- V.unsafeThaw xs
  let n = MV.length v - 1
  quicksort' v 0 n
  return v
  where
    quicksort' v l r =
      if r - l <= 0
      then return ()
      else do
        pivot  <- partition v l r
        quicksort' v l (pivot - 1)
        quicksort' v (pivot + 1) r
    partition v l r = do
      p <- MV.read v l
      iVar <- newSTRef (l + 1)
      forM_ [l + 1 .. r] $ \j -> do
        aj <- MV.read v j
        when (aj < p) $ do
          i <- readSTRef iVar
          MV.swap v j i
          writeSTRef iVar (i + 1)
      i <- readSTRef iVar
      MV.swap v l (i - 1)
      return (i - 1)
