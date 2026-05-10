module Solution where

newtype Reader r a = Reader { runReader :: r -> a }


-- 1. Functor, Applicative, Monad instances
instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f reader = Reader (\env -> f (runReader reader env))


instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure x = Reader (\_ -> x)

    liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
    liftA2 f ra rb = Reader (\env -> f (runReader ra env) (runReader rb env))

instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    ra >>= f = Reader (\env -> runReader (f (runReader ra env)) env)

-- 2. Primitive operations
ask :: Reader r r
ask = Reader (\env -> env)


asks :: (r -> a) -> Reader r a
asks f = Reader (\env -> f env)

local :: (r -> r) -> Reader r a -> Reader r a
local modify reader = Reader (\env -> runReader reader (modify env))


-- 3. Banking system
data BankConfig = BankConfig
    { interestRate   :: Double
    , transactionFee :: Int
    , minimumBalance :: Int
    } deriving (Show)

data Account = Account
    { accountId :: String
    , balance   :: Int
    } deriving (Show)


calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
    rate <- asks interestRate
    return (floor (fromIntegral (balance account) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    fee <- asks transactionFee
    return account { balance = balance account - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    minBal <- asks minimumBalance
    return (balance account >= minBal)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    updated <- applyTransactionFee account
    interest <- calculateInterest account
    meetsMin <- checkMinimumBalance account
    return (updated, interest, meetsMin)
