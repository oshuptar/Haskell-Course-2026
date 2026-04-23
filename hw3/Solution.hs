module Solution (main) where
import qualified Data.Map as Map
import Data.List (permutations)
import Control.Monad (guard)

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map.Map Pos (Map.Map Dir Pos)


move:: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir =
    case Map.lookup pos maze of
        Nothing -> Nothing
        Just directions -> Map.lookup dir directions

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start [] = return start
followPath maze start (step:path) =
    move maze start step >>= (\nextPos -> followPath maze nextPos path)

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start [] = return [start]
safePath maze start (step:path) = do
    nextPos <- move maze start step
    tailPath <- safePath maze nextPos path
    return ([start] <> tailPath)

-- 2.
type Key = Map.Map Char Char
decrypt :: Key -> String -> Maybe String
decrypt key input = traverse (\char -> Map.lookup char key) input

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords _ [] = Just []
decryptWords key (word:sentence) = do
    decryptedWord <- decrypt key word
    decryptedSentence <- decryptWords key sentence
    return ([decryptedWord] <> decryptedSentence)

-- 3.
type Guest = String
type Conflict = (Guest, Guest)

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) =
    (x, xs) : [(y, x:ys) | (y, ys) <- select xs]

safePair :: (Guest, Guest) -> [Conflict] -> Bool
safePair (guest, adjacentGuest) = all (\conflict -> (guest, adjacentGuest) /= conflict && (adjacentGuest, guest) /= conflict)

seatings' :: [Guest] -> [Conflict] -> [[Guest]]
seatings' [] _ = [[]]
seatings' guests conflicts = do
    (guest, rest) <- select guests
    arrangement <- seatings' rest conflicts -- at this point we assume all conflicts before are filtered out
    case arrangement of
        [] -> guard True
        adjacentGuest:_ -> guard (safePair (guest, adjacentGuest) conflicts)
    return (guest:arrangement)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    arrangement <- seatings' guests conflicts
    case arrangement of
        [] -> return arrangement
        [_] -> return arrangement
        _ -> do
            let firstGuest = head arrangement
                lastGuest  = last arrangement
            guard (safePair (firstGuest, lastGuest) conflicts)
            return arrangement

-- 4. Custom Monad
data Result a = Failure String | Success a [String] deriving (Show, Eq)
instance Functor Result where
    -- fmap :: (a->b) -> f a -> f b
    fmap _ (Failure msg) = Failure msg
    fmap tranformation (Success val warnings) = Success (tranformation val) warnings

instance Applicative Result where
    pure val = Success val []
    -- <*> :: f (a->b) -> f a -> f b
    -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 transformation (Success aVal aWarnings) (Success bVal bWarnings) = Success (transformation aVal bVal) (aWarnings <> bWarnings)
    liftA2 _ (Failure aFailure) _ = Failure aFailure -- assume failure have left - precedence
    liftA2 _ _ (Failure bFailure) = Failure bFailure

instance Monad Result where
    return = pure
    -- >>=  m a -> (a -> m b) -> m b
    (Success val warnings) >>= transformation = case transformation val of
                                                    (Success res resWarnings) ->  Success res (warnings <> resWarnings)
                                                    failure -> failure
    (Failure msg) >>= _ = Failure msg


warn :: String -> Result ()
warn warning = Success () [warning]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure ("Age Cannot Be Negative: " <> show age)
    | age > 150 = do
        warn ("Age Is Above 150: " <> show age) -- >>= \_ -> return age
        return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge


main :: IO ()
main = do
    putStrLn "Task1"
    let maze :: Maze = Map.fromList
            [ ((0,0), Map.fromList [(E, (1,0)), (N, (0,1))])
            , ((1,0), Map.fromList [(W, (0,0)), (E, (2,0))])
            , ((2,0), Map.fromList [(W, (1,0)), (N, (2,1))])
            , ((0,1), Map.fromList [(S, (0,0)), (E, (1,1))])
            , ((1,1), Map.fromList [(W, (0,1)), (E, (2,1))])
            , ((2,1), Map.fromList [(W, (1,1)), (S, (2,0))])
            ]

    print $ followPath maze (0,0) [E,E,N,W]
    print $ safePath maze (0,0) [E,E,N,W]
    print $ followPath maze (0,0) [N,S,S]
    print $ safePath maze (0,0) [N,S,S]

    putStrLn "\nTask2"
    let key :: Key = Map.fromList
            [ ('x', 'y')
            , ('y', 'z')
            , ('z', 'a')
            , ('a', 'b')
            , ('b', 'c')
            , ('c', 'd')
            , ('d', 'e')
            ]
    print $ decrypt key "abcdxyz"
    print $ decrypt key "abcdexyz"
    print $ decryptWords key ["abc", "bcd", "xyz"]
    print $ decryptWords key ["abc", "bcde", "xyz"]

    putStrLn "\nTask3"
    let guests :: [Guest] = ["Lannister", "Targaryen", "Stark"]
    let conflicts :: [Conflict] = [ ("Lannister", "Stark")]
    let guests2 :: [Guest] = ["Lannister", "Targaryen", "Stark", "Baratheon"]
    let conflicts2 :: [Conflict] =[ ("Lannister", "Stark"), ("Targaryen", "Baratheon"), ("Lannister", "Targaryen")]
    print $ seatings guests []
    print $ seatings guests conflicts
    print $ seatings guests2 conflicts2
    print $ length (seatings guests2 [])

    putStrLn "\nTask3"
    let ageList1 :: [Int] = [10,20,30,10,20,0]
    let ageList2 :: [Int] = [10,20,30,10,160,20]
    let ageList3 :: [Int] = [10,20,30,10,160,170,20,190, 10]
    let ageList4 :: [Int] = [10,20,30,169,-10,20]
    print $ validateAges ageList1
    print $ validateAges ageList2
    print $ validateAges ageList3
    print $ validateAges ageList4




