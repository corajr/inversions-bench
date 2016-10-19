module QuicksortSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import qualified Data.Vector.Unboxed as V

import Quicksort

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "quicksort" $
    it "sorts like built-in function" $ property $
      \xs -> V.toList (quicksort (V.fromList xs)) === sort xs
