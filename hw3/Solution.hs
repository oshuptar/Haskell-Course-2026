module Solution (main) where
import qualified Data.Map as Map

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


