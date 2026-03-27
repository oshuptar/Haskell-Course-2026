module Solution (main) where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show, Eq)

-- 1:
instance Functor Sequence where
    --fmap ::  (a->b)->Sequence a-> Sequence b
    fmap f (Empty) = Empty
    fmap f (Single a) = Single(f a)
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


main :: IO ()
main = do
    let sequence = Append (Single 1) (Append (Single 2) (Append (Single 3) Empty))
    let sequence_2 = fmap (+1) sequence
    let sequence_3 = sequence <> sequence_2
    print(sequence_2)
    print(seqToList sequence)
    print(seqLength sequence) 
    print(sequence_3)
    print(tailElem 5 sequence_3)
    print(tailElem 4 sequence_3)
