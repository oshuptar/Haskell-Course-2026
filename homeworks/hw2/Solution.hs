module Solution (main) where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show, Eq)

instance Functor Sequence where
    --fmap ::  (a->b)->Sequence a-> Sequence b
    fmap f (Empty) = Empty
    fmap f (Single a) = Single(f a)
    fmap f (Append a1 a2) = Append (fmap f a1) (fmap f a2)

instance Foldable Sequence where
    -- foldmap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap f (Empty) = mempty
    foldMap f (Single a) = f a
    foldMap f (Append a1 a2) = foldMap f a1 <> foldMap f a2

main :: IO ()
main = do
    let sequence = Append (Single 1) (Append (Single 2) (Append (Single 3) Empty))
    print(fmap (+1) sequence)
    print(foldMap (\a -> [a]) sequence)