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
countColors xs = map (countColor xs) colors

countColor :: Code -> Peg -> Int
countColor [] _ = 0
countColor (x:xs) p
  | x == p    = 1 + countColor xs p
  | otherwise = countColor xs p

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
allCodes n = [x:y | x <- colors, y <- allCodes(n-1)]

-- Exercise 7 -----------------------------------------

guessMove :: Code -> [Code] -> Move
guessMove _ [] = error "We're out of possible moves!"
guessMove c cs = getMove c $ head cs

guessMoves :: Code -> [Code] -> [Move]
guessMoves c cs
  | length cs == 1  = []
  | otherwise       = [g] ++ guessMoves c (filterCodes g cs)
    where
      g = guessMove c cs

solve :: Code -> [Move]
solve c = guessMoves c $ allCodes $ length c

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
