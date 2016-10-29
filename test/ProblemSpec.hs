{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module ProblemSpec where

import           ClassyPrelude
import           Test.Hspec
import           Debug.Trace

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

isPalindrome :: Int -> Bool
isPalindrome = isPalindrome' . show

isPalindrome' :: String -> Bool
isPalindrome' x
  | length x <= 1 = True
  | otherwise = if (take 1 x) == (take 1 $ reverse x) then isPalindrome' $ reverse $ drop 1 $ reverse $ drop 1 x else False

problem4 :: Int -> Int -> [Int]
problem4 l r
  | l <= 0 = []
  | r <= 0 = problem4 (l - 1) (l - 1)
  | isPalindrome $ l * r = ([l * r] ++ problem4 l (r - 1))
  | otherwise = problem4 l (r - 1)

problem4' :: Int -> [Int]
problem4' x = take 1 $ reverse $ sort $ problem4 x x

spec :: Spec
spec = do
  describe "problem1" $ do
    it "works!" $ do
      problem1 `shouldBe` 233168
      (take 6 $ fibTo4000000 0 1) `shouldBe` [1, 2, 3, 5, 8, 13]
      problem2 `shouldBe` 4613732
      problem3 1 14 `shouldBe` 7
      problem3 1 600851475143 `shouldBe` 6857
      isPalindrome 1 `shouldBe` True
      isPalindrome 10 `shouldBe` False
      isPalindrome 101 `shouldBe` True
      problem4' 99 `shouldBe` [9009]
      problem4' 999 `shouldBe` [906609]
