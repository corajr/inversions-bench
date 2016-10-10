{-# LANGUAGE ScopedTypeVariables #-}
module InversionsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Inversions
import Data.List (sort)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inversions" $ do
    it "returns ([], 0) for an empty list" $
      inversions ([] :: [Int]) `shouldBe` ([], 0)
    it "returns the sorted array and number of inversions (out-of-order pairs) in a sequence" $ do
      inversions [1,2,3] `shouldBe` ([1,2,3], 0)
      inversions [1,3,2] `shouldBe` ([1,2,3], 1)
    it "returns a sorted array in the first part of the tuple for any input" $ property $
      \(xs :: [Int]) -> fst (inversions xs) === sort xs
    it "returns the sorted array and its length choose 2 if the array is sorted in reverse" $ property $
      \(NonNegative n) -> let xs = [n, n-1 .. 1]
                          in inversions xs === ([1..n], (n*(n-1)) `div` 2)
