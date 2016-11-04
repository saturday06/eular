{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module ProblemSpec where

import           ClassyPrelude
import           Test.Hspec
import           Debug.Trace

problem1 :: Int
problem1 = sum [x | x <- [1..999], x `mod` 5 == 0 || x `mod` 3 == 0]

fibTo4000000 :: Int -> Int -> [Int]
fibTo4000000 x y | y < 4000000 = (x + y):fibTo4000000 y (x + y)
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
  | otherwise = (take 1 x == take 1 (reverse x)) && isPalindrome' (reverse $ drop 1 $ reverse $ drop 1 x)

problem4 :: Int -> Int -> [Int]
problem4 l r
  | l <= 0 = []
  | r <= 0 = problem4 (l - 1) (l - 1)
  | isPalindrome $ l * r = (l * r):problem4 l (r - 1)
  | otherwise = problem4 l (r - 1)

problem4' :: Int -> [Int]
problem4' x = take 1 $ sortBy (flip compare) (problem4 x x)

problem5 :: Int -> Int -> Int
problem5 x y
  | all (\z -> y `mod` z == 0) [1..x] = y
  | otherwise = problem5 x $ y + 1

problem6 :: Int -> Int
problem6 x = sos2 - sos1
  where
    sos1 = sum $ map (\x -> x * x) [1..x]
    sos2 = sum [1..x] * sum [1..x]

problem7 :: Int -> Int -> [Int] -> Int
problem7 l x primes
  | length primes == l = sum $ take 1 primes
  | otherwise = if all (\y -> x `mod` y /= 0) primes then
                  problem7 l (x+1) (x:primes)
                else
                  problem7 l (x+1) primes

arr = [7, 3, 1, 6, 7, 1, 7, 6, 5, 3, 1, 3, 3, 0, 6, 2, 4, 9, 1, 9, 2, 2, 5, 1, 1, 9, 6, 7, 4, 4, 2, 6, 5, 7, 4, 7, 4, 2, 3, 5, 5, 3, 4, 9, 1, 9, 4, 9, 3, 4, 9, 6, 9, 8, 3, 5, 2, 0, 3, 1, 2, 7, 7, 4, 5, 0, 6, 3, 2, 6, 2, 3, 9, 5, 7, 8, 3, 1, 8, 0, 1, 6, 9, 8, 4, 8, 0, 1, 8, 6, 9, 4, 7, 8, 8, 5, 1, 8, 4, 3, 8, 5, 8, 6, 1, 5, 6, 0, 7, 8, 9, 1, 1, 2, 9, 4, 9, 4, 9, 5, 4, 5, 9, 5, 0, 1, 7, 3, 7, 9, 5, 8, 3, 3, 1, 9, 5, 2, 8, 5, 3, 2, 0, 8, 8, 0, 5, 5, 1, 1, 1, 2, 5, 4, 0, 6, 9, 8, 7, 4, 7, 1, 5, 8, 5, 2, 3, 8, 6, 3, 0, 5, 0, 7, 1, 5, 6, 9, 3, 2, 9, 0, 9, 6, 3, 2, 9, 5, 2, 2, 7, 4, 4, 3, 0, 4, 3, 5, 5, 7, 6, 6, 8, 9, 6, 6, 4, 8, 9, 5, 0, 4, 4, 5, 2, 4, 4, 5, 2, 3, 1, 6, 1, 7, 3, 1, 8, 5, 6, 4, 0, 3, 0, 9, 8, 7, 1, 1, 1, 2, 1, 7, 2, 2, 3, 8, 3, 1, 1, 3, 6, 2, 2, 2, 9, 8, 9, 3, 4, 2, 3, 3, 8, 0, 3, 0, 8, 1, 3, 5, 3, 3, 6, 2, 7, 6, 6, 1, 4, 2, 8, 2, 8, 0, 6, 4, 4, 4, 4, 8, 6, 6, 4, 5, 2, 3, 8, 7, 4, 9, 3, 0, 3, 5, 8, 9, 0, 7, 2, 9, 6, 2, 9, 0, 4, 9, 1, 5, 6, 0, 4, 4, 0, 7, 7, 2, 3, 9, 0, 7, 1, 3, 8, 1, 0, 5, 1, 5, 8, 5, 9, 3, 0, 7, 9, 6, 0, 8, 6, 6, 7, 0, 1, 7, 2, 4, 2, 7, 1, 2, 1, 8, 8, 3, 9, 9, 8, 7, 9, 7, 9, 0, 8, 7, 9, 2, 2, 7, 4, 9, 2, 1, 9, 0, 1, 6, 9, 9, 7, 2, 0, 8, 8, 8, 0, 9, 3, 7, 7, 6, 6, 5, 7, 2, 7, 3, 3, 3, 0, 0, 1, 0, 5, 3, 3, 6, 7, 8, 8, 1, 2, 2, 0, 2, 3, 5, 4, 2, 1, 8, 0, 9, 7, 5, 1, 2, 5, 4, 5, 4, 0, 5, 9, 4, 7, 5, 2, 2, 4, 3, 5, 2, 5, 8, 4, 9, 0, 7, 7, 1, 1, 6, 7, 0, 5, 5, 6, 0, 1, 3, 6, 0, 4, 8, 3, 9, 5, 8, 6, 4, 4, 6, 7, 0, 6, 3, 2, 4, 4, 1, 5, 7, 2, 2, 1, 5, 5, 3, 9, 7, 5, 3, 6, 9, 7, 8, 1, 7, 9, 7, 7, 8, 4, 6, 1, 7, 4, 0, 6, 4, 9, 5, 5, 1, 4, 9, 2, 9, 0, 8, 6, 2, 5, 6, 9, 3, 2, 1, 9, 7, 8, 4, 6, 8, 6, 2, 2, 4, 8, 2, 8, 3, 9, 7, 2, 2, 4, 1, 3, 7, 5, 6, 5, 7, 0, 5, 6, 0, 5, 7, 4, 9, 0, 2, 6, 1, 4, 0, 7, 9, 7, 2, 9, 6, 8, 6, 5, 2, 4, 1, 4, 5, 3, 5, 1, 0, 0, 4, 7, 4, 8, 2, 1, 6, 6, 3, 7, 0, 4, 8, 4, 4, 0, 3, 1, 9, 9, 8, 9, 0, 0, 0, 8, 8, 9, 5, 2, 4, 3, 4, 5, 0, 6, 5, 8, 5, 4, 1, 2, 2, 7, 5, 8, 8, 6, 6, 6, 8, 8, 1, 1, 6, 4, 2, 7, 1, 7, 1, 4, 7, 9, 9, 2, 4, 4, 4, 2, 9, 2, 8, 2, 3, 0, 8, 6, 3, 4, 6, 5, 6, 7, 4, 8, 1, 3, 9, 1, 9, 1, 2, 3, 1, 6, 2, 8, 2, 4, 5, 8, 6, 1, 7, 8, 6, 6, 4, 5, 8, 3, 5, 9, 1, 2, 4, 5, 6, 6, 5, 2, 9, 4, 7, 6, 5, 4, 5, 6, 8, 2, 8, 4, 8, 9, 1, 2, 8, 8, 3, 1, 4, 2, 6, 0, 7, 6, 9, 0, 0, 4, 2, 2, 4, 2, 1, 9, 0, 2, 2, 6, 7, 1, 0, 5, 5, 6, 2, 6, 3, 2, 1, 1, 1, 1, 1, 0, 9, 3, 7, 0, 5, 4, 4, 2, 1, 7, 5, 0, 6, 9, 4, 1, 6, 5, 8, 9, 6, 0, 4, 0, 8, 0, 7, 1, 9, 8, 4, 0, 3, 8, 5, 0, 9, 6, 2, 4, 5, 5, 4, 4, 4, 3, 6, 2, 9, 8, 1, 2, 3, 0, 9, 8, 7, 8, 7, 9, 9, 2, 7, 2, 4, 4, 2, 8, 4, 9, 0, 9, 1, 8, 8, 8, 4, 5, 8, 0, 1, 5, 6, 1, 6, 6, 0, 9, 7, 9, 1, 9, 1, 3, 3, 8, 7, 5, 4, 9, 9, 2, 0, 0, 5, 2, 4, 0, 6, 3, 6, 8, 9, 9, 1, 2, 5, 6, 0, 7, 1, 7, 6, 0, 6, 0, 5, 8, 8, 6, 1, 1, 6, 4, 6, 7, 1, 0, 9, 4, 0, 5, 0, 7, 7, 5, 4, 1, 0, 0, 2, 2, 5, 6, 9, 8, 3, 1, 5, 5, 2, 0, 0, 0, 5, 5, 9, 3, 5, 7, 2, 9, 7, 2, 5, 7, 1, 6, 3, 6, 2, 6, 9, 5, 6, 1, 8, 8, 2, 6, 7, 0, 4, 2, 8, 2, 5, 2, 4, 8, 3, 6, 0, 0, 8, 2, 3, 2, 5, 7, 5, 3, 0, 4, 2, 0, 7, 5, 2, 9, 6, 3, 4, 5, 0]

problem8 :: Int -> Int -> Int -> Int
problem8 x offset max
  | length arr < offset = max
  | otherwise = problem8 x (offset + 1) (if cur > max then cur else max)
  where cur = product $ take x $ drop offset arr

problem9 :: [Int]
problem9 = do --[ (trace ("a=" ++ a ++ ", b=" ++ b ++ ", c=" ++ c) a+b+c) | a <- [1..1000], b <- [1..1000], c <- [1..1000], ]
  a <- [1..333]
  b <- [a..333]
  c <- [b..333]
  guard $ a*a + b*b == c*c
  guard $ a + b + c == 1000
  return $ a * b * c --"a=" ++ show a ++ ", b=" ++ show b ++ ", c=" ++ show c

problem10 :: Int -> Int -> [Int] -> [Int]
problem10 below x primes
  | cur >= below = drop 1 primes
  | otherwise =
    if all (\y -> x `mod` y /= 0) primes then
      problem10 below (x+1) (x:primes)
    else
      problem10 below (x+1) primes
  where cur = sum $ take 1 primes

spec :: Spec
spec =
  describe "problem1" $
    it "works!" $ do
      sum (problem10 (2*1000 * 1000) 2 []) `shouldBe` 0
      sum (problem10 10 2 []) `shouldBe` 17
      -- problem9 `shouldBe` [31875000]
      -- problem8 13 0 0 `shouldBe` 5832
      problem8 4 0 0 `shouldBe` 5832
      -- problem7 10001 2 [2] `shouldBe` 13
      -- problem7 6 2 [2] `shouldBe` 13
      problem6 100 `shouldBe` 25164150
      problem6 10 `shouldBe` 2640
      problem5 10 1 `shouldBe` 2520
      -- problem5 20 1 `shouldBe` -1
      problem1 `shouldBe` 233168
      take 6 (fibTo4000000 0 1) `shouldBe` [1, 2, 3, 5, 8, 13]
      problem2 `shouldBe` 4613732
      problem3 1 14 `shouldBe` 7
      problem3 1 600851475143 `shouldBe` 6857
      isPalindrome 1 `shouldBe` True
      isPalindrome 10 `shouldBe` False
      isPalindrome 101 `shouldBe` True
      problem4' 99 `shouldBe` [9009]
      problem4' 999 `shouldBe` [906609]
