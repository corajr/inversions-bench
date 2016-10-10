module Main where

import Inversions
import Criterion.Main
import qualified Data.Sequence as S

ex :: Int -> [Int]
ex n = [n, n-1 .. 1]

ex1 = ex 10000

ex1Seq = S.fromList ex1

main :: IO ()
main = defaultMain [
    bgroup "inversionsList"
      [ bench "[10000, 9999 .. 1]" $ whnf inversionsList ex1
      ]
  , bgroup "inversionsSeq"
      [ bench "[10000, 9999 .. 1]" $ whnf inversionsSeq ex1Seq
      ]
  , bgroup "inversionsGeneric"
      [ bench "[10000, 9999 .. 1] (List)" $ whnf inversionsGeneric ex1
      , bench "[10000, 9999 .. 1] (Seq)" $ whnf inversionsGeneric ex1Seq
      ]
  ]
