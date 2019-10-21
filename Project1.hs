module Project1 where

-- triples returns the number of items in a list that are divisible by 3.
--
-- > triples [1,2,3,4,5,6]
-- 2
-- > triples [3,33,333]
-- 3
-- > triples [0,1,4]
-- 1

triples :: [Int] -> Int
triples [] = 0
triples (x:xs) = if x `mod` 3 == 0 then 1 + triples xs else 0 + triples xs


-- The hailstone sequence takes a positive number n and repeatedly applies
-- this transformation: if n is even, it divides n by 2; otherwise, it
-- multiplies n by 3 and adds one. The sequence ends when it reaches 1.
--
-- hailstone returns the complete sequence beginning with a particular number.
-- You may assume that the number is positive.
--
-- > hailstone 4
-- [4,2,1]
-- > hailstone 6
-- [6,3,10,5,16,8,4,2,1]
-- > hailstone 7
-- [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]

hailstone :: Integer -> [Integer]
hailstone x = if x == 1 then [1] else if x `mod` 2 == 0 then x: hailstone(quot x 2) else x: hailstone (3 * x + 1)


data Point = Pt Double Double deriving (Show, Eq)

-- The centroid of a list of points is a point whose x (and y) coordinates are
-- the means of the x (and y) coordinates of the points in the list.
--
-- You may assume the list contains at least one point.
--
-- > centroid [Pt 1 1, Pt 2 2]
-- Pt 1.5 1.5
-- > centroid [Pt (-1.5) 0, Pt 3 2, Pt 0 1]
-- Pt 0.5 1.0

addPoints :: Point -> Point -> Point
addPoints (Pt x y) (Pt z w) = Pt (x + z) (y + w)


getMean :: [Point] -> Point -> Point
getMean (xs) (Pt x y) = Pt (x / fromIntegral (length xs)) (y / fromIntegral (length xs))


getSum :: [Point] -> Point
getSum [] = Pt 0 0
getSum (x:xs) = addPoints x (getSum xs)

centroid :: [Point] -> Point
centroid [] = Pt 0 0
centroid (xs) = getMean xs (getSum xs)

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)


-- mirror returns a tree with the same shape and contents as its argument, but
-- flipped left for right.
--
-- > mirror (Bin (Bin Tip 1 (Bin Tip 2 Tip)) 3 Tip)
-- Bin Tip 3 (Bin (Bin Tip 2 Tip) 1 Tip)

mirror :: Tree a -> Tree a
mirror Tip = Tip
mirror (Bin l d r) = Bin (mirror r) d (mirror l)

-- In a strictly binary tree, each node has either 0 children or 2 children.
--
-- > strict (Bin Tip 1 Tip)
-- True
-- > strict (Bin Tip 1 (Bin Tip 2 Tip))
-- False

strict :: Tree a -> Bool
strict Tip = True
strict (Bin Tip _ Tip) = True
strict (Bin l _ Tip) = False
strict (Bin Tip _ r) = False
strict (Bin l _ r) = (&&) (strict l) (strict r)


-- A tree is near balanced if the left and right sub-trees of each node differ
-- in height by at most 1.
--
-- > near_balanced (Bin Tip 1 (Bin Tip 2 Tip))
-- True
-- > near_balanced (Bin Tip 1 (Bin Tip 2 (Bin Tip 3 Tip)))
-- False
-- > near_balanced (Bin (Bin Tip 2 Tip) 1 (Bin Tip 2 (Bin Tip 3 Tip)))
-- True
-- > near_balanced (Bin (Bin Tip 2 Tip) 1 (Bin Tip 2 (Bin (Bin Tip 4 Tip) 3 Tip)))
-- False

getRightHeight :: Tree a -> Int
getRightHeight Tip = 0
getRightHeight (Bin l _ r) = (max (getRightHeight l) (getRightHeight r)) +1

getLeftHeight :: Tree a -> Int
getLeftHeight Tip = 0
getLeftHeight (Bin l _ r) = (max (getLeftHeight l) (getLeftHeight r)) +1

near_balanced :: Tree a -> Bool
near_balanced (Bin l _ r) = abs ((getLeftHeight l) - (getRightHeight r)) <= 1
