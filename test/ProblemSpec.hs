{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module ProblemSpec where

import           ClassyPrelude
import           Test.Hspec

problem1 :: Int
problem1 = foldr (+) 0 [x | x <- [1..999], x `mod` 5 == 0 || x `mod` 3 == 0]

fibTo4000000 :: Int -> Int -> [Int]
fibTo4000000 x y | y < 4000000 = (x + y):(fibTo4000000 y (x + y))
fibTo4000000 _ _ = []

problem2 :: Int
problem2 = sum [x | x <- fibTo4000000 0 1, x `mod` 2 == 0]

problem3 :: Int -> Int -> Int
problem3 child mother
  | child >= mother = mother
  | mother `mod` child == 0 = problem3 (child + 1) (mother `div` child)
  | otherwise = problem3 (child + 1) mother

spec :: Spec
spec = do
  describe "problem1" $ do
    it "works!" $ do
      problem1 `shouldBe` 233168
      (take 6 $ fibTo4000000 0 1) `shouldBe` [1, 2, 3, 5, 8, 13]
      problem2 `shouldBe` 4613732
      problem3 1 14 `shouldBe` 7
      problem3 1 600851475143 `shouldBe` 6857
