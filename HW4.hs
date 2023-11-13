-- CS 357
-- Hugh McFall
-- Homework 4
-- 11/13/23
-- Problem 1

data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Show, Eq)

-- This is the splitlist helper function it just takes a list and turns it into
-- a tuple of two lists of equal (or one off) length

splitList :: [a] -> ([a], [a])
splitList xs = splitAt (length xs `div` 2) xs

-- This is the actual balance function which takes a list and turns it inmto
-- a balanced tree. 

balance :: [a] -> Tree a
balance [x] = LeafT x -- if the list has one element make a leaf
balance xs = NodeT (balance left) (balance right) --otherwise split the 
--list and run balance on sublists
    where --splitlist call
        (left, right) = splitList xs

-- Problem 2

data T = Leaf | Node T T deriving (Eq, Show)

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

-- The allpaths function takes a tree T and returns a path P through the tree. 
allpaths :: T -> [P]
allpaths Leaf = [This]--if the function receives a Leaf it just returns this 
    --Otherwise if the function receives a node on a tree it returns the
    --concatinated path of both branches from the node
allpaths (Node left right) = This : (map GoLeft (allpaths left) ++ map GoRight (allpaths right))

--examples for testing with ghci
example2 :: T
example2 = Node Leaf (Node Leaf Leaf)

example3 :: T
example3 = Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf))

-- Problem 3

data Expr = Val Int | Add Expr Expr
    deriving (Eq, Show)

--folde folds with an expression in, this case add
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x --folde's base takes an expression and the value x and
--runs that value through that expression
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2) --if it gets the add
--call and two expressions it runs the folde on e1 and e2

--the eval helper function evaluates the expressions using the folde
eval :: Expr -> Int
eval expr = folde id (+) expr

exprExample1 :: Expr
exprExample1 = Add (Val 1) (Val 2)

exprExample2 :: Expr
exprExample2 = Add (Add (Val 1) (Val 2)) (Val 3)

-- Problem 4

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = [] --anything passed with emptylist should just return an
--empty list
myTakeWhile p (x:xs) -- if passed a predicate and a list it should 
    | p x       = x : myTakeWhile p xs
    | otherwise = []

-- Problem 5

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan _ [] = ([], []) --if passed an empty list it just returns an empty tuple
mySpan p (x:xs)--otherwise uses the predicate p and iterates over the list. 
    | p x       = let (taken, rest) = mySpan p xs--checks if the predicate is
        --satisfied and if so adds it to the list
                  in (x : taken, rest)
    | otherwise = ([], x:xs)--otherwise it tuples the list with the rest of the
    --list

-- predicates and lists for testing:
predicate1 = (\x -> x < 5)
predicate2 = (\x -> x /= ' ')
list1 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
list2 = "Hello World!"

-- Problem 6

combinations3 :: Ord a => [a] -> [[a]]
combinations3 = undefined

-- Problem 7

increasing :: Ord a => [a] -> Bool
increasing = undefined

-- Problem 8

combinations :: (Ord a, Integral b) => b -> [a] -> [[a]]
combinations = undefined

-- Problem 9

data Complex = Complex { real :: Integer, imaginary :: Integer }
