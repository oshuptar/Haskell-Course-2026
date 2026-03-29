module Solution (main) where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show, Eq)

-- 1:
instance Functor Sequence where
    --fmap ::  (a->b)->Sequence a-> Sequence b
    fmap f (Empty) = Empty
    fmap f (Single a) = Single (f a)
    fmap f (Append a1 a2) = Append (fmap f a1) (fmap f a2)

-- 2:
instance Foldable Sequence where
    -- foldmap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap f (Empty) = mempty
    foldMap f (Single a) = f a
    foldMap f (Append a1 a2) = foldMap f a1 <> foldMap f a2

seqToList:: Sequence a -> [a]
seqToList a = foldMap (\x -> [x]) a

seqLength:: Sequence a -> Int
seqLength a = foldl (\acc _ -> acc + 1) 0 a -- first argument is accumulator second argument is element

-- 3:
instance Monoid (Sequence a) where
    mempty = Empty

instance Semigroup (Sequence a) where
    -- (<>) :: Sequence a -> Sequence a -> Sequence a
    (<>) (Empty) seq2 = seq2
    (<>) seq1 (Empty) = seq1
    (<>) (Single a) seq2 = Append (Single a) (seq2)
    (<>) (Append subseq1 subseq2) seq2 = subseq1 <> (subseq2 <> seq2)

-- 4:

tailElem :: Eq a => a -> Sequence a -> Bool
tailElem a (Empty) = False
tailElem a (Single val) = a == val
tailElem a (Append seq1 seq2) = go a [seq1, seq2] False
    where
        go :: Eq a => a -> [Sequence a] -> Bool -> Bool
        go _ _ True = True
        go a [] acc = acc
        go a ((Append seq1_1 seq1_2):seq2) acc = go a (seq1_1:seq1_2:seq2) acc
        go a ((Empty):seq2) acc = go a (seq2) acc
        go a ((Single val):seq2) _ = go a (seq2) (a == val)


-- 5:
tailToList::Sequence a -> [a]
tailToList Empty = []
tailToList (Single val) = [val]
tailToList seq = go [] [seq]
    where
        go :: [a] -> [Sequence a] -> [a]
        go list [] = list
        go list ((Append seq1_1 seq1_2):seq2) = go list (seq1_1:seq1_2:seq2)
        go list ((Single val):seq2) = go (list ++ [val]) seq2
        go list ((Empty):seq2) = go list seq2

-- 6:
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN [] = Nothing
tailRPN expr = go expr []
    where
        go :: [Token] -> [Int] -> Maybe Int
        go [] [val] = Just val
        go [] _ = Nothing
        go ((TNum val):expr) stack = go expr (val:stack)
        go ((TAdd):expr) (right:left:stack) = go expr ((left + right):stack)
        go ((TSub):expr) (right:left:stack) = go expr ((left - right):stack)
        go ((TMul):expr) (right:left:stack) = go expr ((left * right):stack)
        go ((TDiv):expr) (0:left:stack) = Nothing
        go ((TDiv):expr) (right:left:stack) = go expr ((left `div` right):stack)
        go _ _ = Nothing

-- 7:
-- foldl::(b -> a -> b) -> b -> t a -> b
-- foldr:: (a -> b -> b) -> b -> t a -> b
myReverse:: [a] -> [a]
myReverse [] = []
myReverse list = foldl (\seed elem -> elem:seed) [] list

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile predicate list = foldr (\elem seed ->
    if predicate elem
    then elem:seed
    else []) [] list

decimal :: [Int] -> Int
decimal [] = 0
decimal list = foldl (\seed elem -> 10*seed + elem) 0 list


main :: IO ()
main = do
    let sequence = Append (Single 1) (Append (Single 2) (Append (Single 3) Empty))
    let sequence_2 = fmap (+1) sequence
    let sequence_3 = sequence <> sequence_2
    let sequence_4 = Append (Append (Single 10) Empty) (Append (Single 9) (Append Empty (Single 8)))
    let empty_sequence = Empty :: Sequence Int
    print (sequence_2)
    print (seqToList sequence)
    print (seqLength sequence)
    print (sequence_3)
    print (tailElem 5 sequence_3)
    print (tailElem 4 sequence_3)
    print (tailToList sequence_3)
    print (tailToList empty_sequence)
    print (tailToList sequence_4)
    print $ tailRPN [TNum 2, TNum 3, TAdd]
    print $ tailRPN [TNum 5, TNum 2, TSub]
    print $ tailRPN [TNum 4, TNum 3, TMul]
    print $ tailRPN [TNum 20, TNum 5, TDiv]
    print $ tailRPN [TNum 2, TNum 3, TNum 4, TAdd, TMul]
    print $ tailRPN [TNum 10, TNum 2, TNum 3, TMul, TAdd]
    print $ tailRPN [TNum 8, TNum 0, TDiv]
    print $ tailRPN [TAdd]
    print $ tailRPN []
    print $ myReverse [1,2,3,4,5]
    print $ myTakeWhile even [2,4,10,11,17,18]
    print $ myTakeWhile even [3,4,10,11,17,18]
    print $ myTakeWhile (<5) [6,7,1,2,3,4]
    print $ myTakeWhile (<5) [1,2,3,4, 6, 7]
    print $ decimal []
    print $ decimal [0,2,0,1,4]
    print $ decimal [1,2,3,4]


