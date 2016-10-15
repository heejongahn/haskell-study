{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n
  | n `elem` (0, 1) = 1
  | otherwise       = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) fibs2 $ tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x nextStream) = x : streamToList nextStream

-- Exercise 4 -----------------------------------------

instance Functor Stream where
  fmap f (Cons x nextStream) = Cons (f x) (fmap f nextStream)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a b) second = Cons a (sInterleave second b)

sTake :: Int -> Stream a -> [a]
sTake n (Cons a b)
  | n == 0 = []
  | otherwise = a : sTake (n-1) b

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) $ fmap (+1) $ ruler

-- Exercise 7 -----------------------------------------

next :: Int -> Int
next n = (110351245 * n + 12345) `mod` 2147483648

-- | Implementation of C rand
rand :: Int -> Stream Int
rand r0 = Cons r0 (rand $ next r0)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax [x] = Just (x, x)
minMax (x:xs) = fmap (\(pMin, pMax) -> ((min pMin x), (max pMax x))) $ minMax xs

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
