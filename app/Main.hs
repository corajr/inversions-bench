module Main where

import Inversions
import Criterion.Main
import qualified Data.Sequence as S

ex :: Int -> [Int]
ex n = [n, n-1 .. 1]

main :: IO ()
main = defaultMain [
    bgroup "inversionsList"
      [ bench "[10000, 9999 .. 1]" $ whnf inversionsList (ex 10000)
      ]
  , bgroup "inversionsSeq"
      [ bench "[10000, 9999 .. 1]" $ whnf inversionsSeq (S.fromList (ex 10000))
      ]
  ]
