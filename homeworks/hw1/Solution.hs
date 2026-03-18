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


-- 2. **Coprime Pairs**
--  Write a function `coprimePairs :: [Int] -> [(Int, Int)]` that takes a list of positive integers
--  and returns all unique pairs `(x, y)` (with `x < y`) for which `gcd x y == 1`. You may use Haskell's built-in `gcd`.

removeDuplicates :: Eq a => [a]->[a]
removeDuplicates xs = reverse (go xs [])
    where
        go [] acc = acc
        go (x:xs) !acc = if 
            all (\y -> y /= x) xs 
            then go xs (x : acc)
            else go xs acc
-- since appending to the end of the list is O(n) on each step. We reverse it once at the end with complexity O(n)

-- the complexity of generating pairs is O(n^2), hence the function to remove duplicates from the list would not asymptotically affect the algorithm
-- removeDuplicates is optional, if we allow duplicate values in the list
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs xs
    | any ( <= 0) xs = []
    | otherwise = go (removeDuplicates xs) 
 where
    go [] = []
    go (x:xs) = [(min x y, max x y) | y <- xs , x /= y ,x `gcd` y == 1] ++ go xs 
    -- we handle the case x /= y, because if x = 1 and y = 1 => gcd x y == 1, but x is not < y

-- 3. Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x/= 0]

primesTo :: Int -> [Int]
primesTo n 
    | n < 2 = []
    | otherwise = sieve [2..n]

isPrime' :: Int -> Bool
isPrime' n = any (== n) (primesTo n)

-- 4. Matrix Multiplication

-- we implicitly assume that the dimensions are correct
-- no matrix dimension checks are done
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] _ = []
matMul _ [] = []
matMul a b = [
    [ sum [ a !! i !! k * b !! k !! j | k <- [0..p-1]]
    | j <- [0..n-1]]
    | i <- [0..m-1]]
    where 
        m = length a
        p = length (head a)
        n = length (head b)

