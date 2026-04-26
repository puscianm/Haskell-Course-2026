module Solutions where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (guard)
import Control.Monad.Writer

-- MAYBE MONAD 

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)
type Key = Map Char Char

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    neighbours <- Map.lookup pos maze
    Map.lookup dir neighbours

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = Just pos
followPath maze pos (d:ds) = do
    next <- move maze pos d
    followPath maze next ds

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = Just [pos]
safePath maze pos (d:ds) = do
    next <- move maze pos d
    rest <- safePath maze next ds
    return (pos : rest)

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (\c -> Map.lookup c key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)


-- LIST MONAD

type Guest = String
type Conflict = (Guest, Guest)

picks :: [a] -> [(a, [a])]
picks [] = []
picks (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- picks xs]

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = do
    (x, rest) <- picks xs
    perm <- perms rest
    return (x : perm)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    arrangement <- perms guests
    guard (all allowed (adjacencies arrangement))
    return arrangement
  where
    adjacencies arr = zip arr (tail arr ++ [head arr])
    allowed (a, b) = (a, b) `notElem` conflicts
     && (b, a) `notElem` conflicts


-- CUSTOM MONAD

data Result a = Failure String | Success a [String]

instance Show a => Show (Result a) where
    show (Failure msg) = "Failure: " ++ msg
    show (Success val ws) = "Success: " ++ show val
                         ++ if null ws then ""
                            else "\nWarnings: " ++ show ws

instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success val ws) = Success (f val) ws

instance Applicative Result where
    pure x = Success x []
    Failure msg <*> _ = Failure msg
    _ <*> Failure msg = Failure msg
    Success f ws1 <*> Success x ws2 = Success (f x) (ws1 ++ ws2)

instance Monad Result where
    return = pure
    Failure msg >>= _ = Failure msg
    Success val ws >>= f = case f val of
        Failure msg -> Failure msg
        Success val' ws' -> Success val' (ws ++ ws')

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure "Age cannot be less then 0"
    | age > 150 = do warn ("Age cannot be higher then 150. Current age: " ++ show age)
                     return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge


-- WRITER MONAD

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr

instance Show Expr where
    show (Lit n) = show n
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Neg e) = "(-" ++ show e ++ ")"

simplifiedTo :: String -> Expr -> Writer [String] Expr
simplifiedTo rule expr = do
    tell [rule ++ " -> " ++ show expr]
    return expr

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)

simplify (Neg e) = do
    e' <- simplify e
    case e' of
        Neg inner -> simplifiedTo "Double negation: -(-e)"   inner
        _ -> return (Neg e')

simplify (Add a b) = do
    a' <- simplify a
    b' <- simplify b
    case (a', b') of
        (Lit 0, _) -> simplifiedTo "Add identity: 0 + e"     b'
        (_, Lit 0) -> simplifiedTo "Add identity: e + 0"     a'
        (Lit x, Lit y) -> simplifiedTo "Constant folding: a + b" (Lit (x + y))
        _ -> return (Add a' b')

simplify (Mul a b) = do
    a' <- simplify a
    b' <- simplify b
    case (a', b') of
        (Lit 0, _) -> simplifiedTo "Zero absorption: 0 * e"  (Lit 0)
        (_, Lit 0) -> simplifiedTo "Zero absorption: e * 0"  (Lit 0)
        (Lit 1, _) -> simplifiedTo "Mul identity: 1 * e"     b'
        (_, Lit 1) -> simplifiedTo "Mul identity: e * 1"     a'
        (Lit x, Lit y) -> simplifiedTo "Constant folding: a * b" (Lit (x * y))
        _ -> return (Mul a' b')
