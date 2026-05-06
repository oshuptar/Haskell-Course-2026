module Solution(main) where
-- The Reader Monad
newtype Reader r a = Reader { runReader :: r -> a }

-- The Reader monad represents computations that can read values from a shared environment.
-- r is read0only environment threaded implicitly through the computation

instance Functor (Reader r) where
    -- fmap :: (a->b) -> Reader r a -> Reader r b
    fmap f reader =  Reader (\env -> f (runReader reader env))

instance Applicative (Reader r) where
    -- pure :: a -> Reader r a
    pure val = Reader (const val)
    -- liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
    liftA2 f reader1 reader2 = Reader (\env ->  f (runReader reader1 env) (runReader reader2 env))

instance Monad (Reader r) where
    -- >>= :: Reader r a -> (a -> Reader r b) -> Reader r b
    reader >>= f =  Reader (\env -> runReader (f (runReader reader env)) env)

-- 2:
ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader (\env -> f env)

local :: (r -> r) -> Reader r a -> Reader r a
local envModifier reader = Reader (\env -> runReader reader (envModifier env))

-- 3:


main :: IO ()
main = do
    undefined