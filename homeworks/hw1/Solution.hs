module Solution where
import Data.Function

-- 1. Write a function `goldbachPairs :: Int -> [(Int, Int)]` that, given an even integer `n ≥ 4`, returns all pairs `(p, q)` satisfying:
--   - `p` and `q` are both prime numbers
--   - `p + q == n`
--   - `p ≤ q`
--  Use a list comprehension to generate the result. Define a helper `isPrime :: Int -> Bool` using Exercise 3.

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | even n = False
    | otherwise = all (/= 0) [n `mod` i | i <- [2..s]]
    where
        s = n & fromIntegral & sqrt & floor

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
    | n < 4 || odd n = [] 
    | otherwise = [(p, q) | q <- [ceiling (fromIntegral n / 2)..n], let p = n - q, isPrime p, isPrime q]