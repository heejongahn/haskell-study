{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys)
  | x == y    = 1 + exactMatches xs ys
  | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (checkColors xs) colors

checkColors :: Code -> Peg -> Int
checkColors [] _ = 0
checkColors (x:xs) p
  | x == p = 1 + checkColors xs p
  | otherwise = checkColors xs p


-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = sum $ map (uncurry min) $ zip (countColors x) (countColors y)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove x y = Move y exact nonExact
  where
    exact = exactMatches x y
    nonExact = (matches x y) - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c1 e n) c2 = (getMove c2 c1) == (Move c1 e n)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter (isConsistent m) cs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = map (\x -> [x]) colors
allCodes n = foldr (++) [] $ map (concatMap (allCodes (n-1))) colors
  where concatMap :: [Code] -> Peg -> [Code]
        concatMap c p = map ((++) [p]) c

-- Exercise 7 -----------------------------------------

guess :: Code -> [Code] -> Move
guess c [] = undefined
guess c cs = getMove c $ head cs

guessList :: Code -> [Code] -> [Move]
guessList c cs
  | length cs == 1  = []
  | otherwise       = [g] ++ guessList c (filterCodes g cs)
    where
      g = guess c cs

solve :: Code -> [Move]
solve c = guessList c $ allCodes $ length c

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
