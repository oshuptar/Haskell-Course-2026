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
-- asks f = Reader (\env -> f env)
asks f = do f <$> ask

local :: (r -> r) -> Reader r a -> Reader r a
-- local envModifier reader = Reader (\env -> runReader reader (envModifier env))
local envModifier reader = do
    env <- ask
    let localEnv = envModifier env
    return (runReader reader localEnv)


-- 3:
data BankConfig = BankConfig {
    interestRate :: Double,
    transactionFee :: Int,
    minimumBalance :: Int
} deriving (Show)

data Account = Account {
    accountId :: String,
    balance :: Int
} deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
    rate <- asks interestRate
    let accountBalance = balance account
    return (round (rate * fromIntegral accountBalance))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    fee <- asks transactionFee
    return (Account (accountId account) (balance account - fee))

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    threshold <- asks minimumBalance
    return (balance account >= threshold)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    updated <- applyTransactionFee account
    interest <- calculateInterest account
    minBalanceFlag <- checkMinimumBalance account
    return (updated, interest, minBalanceFlag)

main :: IO ()
main = do
    let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 999 }
    let acc = Account { accountId = "A-001", balance = 1000 }
    print $ runReader (processAccount acc) cfg