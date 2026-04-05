import Data.Foldable (toList)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a) deriving(Show)

instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty 
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = (foldMap f l) <> (foldMap f r)

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

instance Semigroup (Sequence a) where
    (<>) :: Sequence a -> Sequence a -> Sequence a
    (<>) a b = Append a b

instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty


tailElem :: Eq a => a -> Sequence a -> Bool
tailElem a s = go [s]
    where
    go [] = False
    go (Empty : rest) = go rest
    go (Single l : r)
        | l == a    = True
        | otherwise = go r
    go ((Append l r) : rest) = go (l : r : rest)

tailToList :: Sequence a  -> [a]
tailToList s = reverse (go [s] [])
    where
    go [] acc = acc
    go (Empty : rest) acc = go rest acc
    go (Single x : rest) acc = go rest (x : acc)
    go ((Append l r) : rest) acc = go (l : r : rest) acc


data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _        = Nothing
    go (TNum n : rest) stack = go rest (n : stack)
    go (TAdd : rest) (b : a : stack) = go rest (a + b : stack)
    go (TSub : rest) (b : a : stack) = go rest (a - b : stack)
    go (TMul : rest) (b : a : stack) = go rest (a * b : stack)
    go (TDiv : rest) (b : a : stack)
        | b == 0    = Nothing
        | otherwise = go rest (a `div` b : stack)
    go _ _ = Nothing


myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

decimal :: [Int] -> Int
decimal = foldl (\acc x -> acc * 10 + x) 0


encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x ((y, n) : rest)
        | x == y    = (y, n + 1) : rest
    step x acc      = (x, 1) : acc

decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []
