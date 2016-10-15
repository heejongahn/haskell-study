{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

removeTrailingZeros :: (Num a, Eq a) => [a] -> [a]
removeTrailingZeros [] = []
removeTrailingZeros xs
  | length xs > 1 && last xs == 0  = removeTrailingZeros $ init xs
  | otherwise                      = xs

instance (Num a, Eq a) => Eq (Poly a) where
  (==) (P xs) (P ys) = (==) (removeTrailingZeros xs) (removeTrailingZeros ys)

-- Exercise 3 -----------------------------------------

mapTermShow :: (Num a, Eq a, Show a) => [a] -> [String]
mapTermShow [] = []
mapTermShow p@(y:ys) = [(singleTermShow y ((-) (length p) 1))] ++ mapTermShow ys

singleTermShow :: (Num a, Eq a, Show a) => a -> Int -> String
singleTermShow coef deg
  | coef == 0 = "0"
  | deg == 0 = show coef
  | deg == 1 && coef == 1 = "x"
  | deg == 1 = (show coef) ++ "x"
  | coef == 1 = "x^" ++ (show deg)
  | otherwise = (show coef) ++ "x^" ++ (show deg)

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P []) = ""
  show (P (xs)) = foldl (\x' y -> x' ++ " + " ++ y) highest rest
    where
      terms = (reverse (removeTrailingZeros xs))
      highest = singleTermShow (head terms) ((-) (length terms) 1)
      rest = filter (\y -> y /= "0") (mapTermShow $ tail terms)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a') (P b') = (P (plus' a' b'))
  where
    plus' [] vs       = vs
    plus' ts []       = ts
    plus' (t:ts) (v:vs) = (t+v) : plus' ts vs

-- Exercise 5 -----------------------------------------

pad :: Num a => Int -> [a] -> [a]
pad deg coefs = (replicate deg 0) ++ coefs

times :: Num a => Poly a -> Poly a -> Poly a
times (P a') (P b') = foldl plus (P [0]) (times' 0 a' b')
  where
    times' _ _ [] = [(P [0])]
    times' _ [] _ = [(P [0])]
    times' deg (a:as) b = (P (pad deg (map (*a) b))) : (times' (deg + 1) as b)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P $ map negate a
    fromInteger y = P [fromInteger y]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P as) val = go 0 ps val
  where
    ps = zip as [0..]
    go acc [] val = acc
    go acc ((deg, coef):rest) val = acc + (coef * (val^deg)) + go acc rest val

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

