module Main where

import Inversions
import Control.Monad (replicateM)
import qualified Data.Sequence as S

ex :: Int -> [Int]
ex n = [n, n-1 .. 1]

ex1 = ex 10000
ex1Seq = S.fromList ex1

main :: IO ()
main = do
  let (_, x) = inversionsList ex1
      (_, y) = inversionsSeq ex1Seq
      (_, z) = inversionsListAndLength (ex1, length ex1)
  print (x, y, z)
