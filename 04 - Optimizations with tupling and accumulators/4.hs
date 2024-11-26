import Data.Char
import Data.List

-- 1) define a function that returns a tuple (String, String) s.t. the first element has all the letters and the second element has all the digits of a given String
digitAlpha :: String -> (String, String)
{- definition with function composition:
digitAlpha [] = ([], [])
digitAlpha (s:ss)
  | isAlpha s = addListPair ([s], []) (digitAlpha ss)
  | isDigit s = addListPair ([], [s]) (digitAlpha ss)
  where
    addListPair :: ([a], [a]) -> ([a], [a]) -> ([a], [a])
    addListPair (x1, x2) (y1, y2) = (x1 ++ y1, x2 ++ y2)

definition with accumulating parameter: -}
digitAlpha [] = ([], [])
digitAlpha l = digitAlpha' l ([], [])
  where
    digitAlpha' :: String -> (String, String) -> (String, String)
    digitAlpha' [] acc = acc
    digitAlpha' (s:ss) (l, n)
        | isAlpha s = digitAlpha' ss (s : l, n)
        | isDigit s = digitAlpha' ss (l, s : n)

-- 2) define a function that returns a triple (Int, Int, Int) s.t. the first element is the amount of negative values, the second is the amount of zeroes, and the third is the amount of positive values on a given [Int] list
nzp :: [Int] -> (Int, Int, Int)
{- definition with function composition:
nzp [] = (0, 0, 0)
nzp (x:xs)
  | x < 0  = addTriple (1, 0, 0) (nzp xs)
  | x == 0 = addTriple (0, 1, 0) (nzp xs)
  | x > 0  = addTriple (0, 0, 1) (nzp xs)
  where
    addTriple :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
    addTriple (x1, x2, x3) (y1, y2, y3) = (x1 + y1, x2 + y2, x3 + y3)

definition with accumulating parameter: -}
nzp l = nzp' l (0, 0, 0)
  where
    nzp' :: [Int] -> (Int, Int, Int) -> (Int, Int, Int)
    nzp' [] acc = acc
    nzp' (x:xs) (f, s, t)
        | x < 0 = nzp' xs (f + 1, s, t)
        | x == 0 = nzp' xs (f, s + 1, t)
        | x > 0 = nzp' xs (f, s, t + 1)

-- 3) define a function that computes a division through recursive subtraction and returns a pair (a, b) s.t. a = the integer division and b = the remainder of the division
{- definition with function composition:
divMod1 :: Integral a => a -> a -> (a, a)
divMod1 a b
  | b > a     = (0, (b - a))
  | otherwise = addPair (1, 0) (divMod1 (a - b) b)
  where
    addPair :: Num a => (a, a) -> (a, a) -> (a, a)
    addPair (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

definition with accumulating parameter: -}
divMod1 :: Integral a => a -> a -> Maybe (a, a)
divMod1 _ 0 = Nothing
divMod1 a b = Just (divMod1' b (0, a))
  where
    divMod1' :: Integral a => a -> (a, a) -> (a, a)
    divMod1' b (q, r)
        | b > r = (q, r)
        | otherwise = divMod1' b (q + 1, r - b)

-- 4) using an auxiliary function with an accumulating parameter, optimize the following recursive definition that returns the number represented by a list of digits
{-
fromDigits :: [Int] -> Int
fromDigits []    = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t
-}
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits s = fromDigits' s 0
  where
    fromDigits' :: [Int] -> Int -> Int
    fromDigits' [] acc = acc
    fromDigits' (h:t) acc = fromDigits' t (h + (10 * acc))

-- 5) using an auxiliary function with accumulating parameters, optimize the following definition that returns the sum of the initial segments of a list with maximum sum
{-
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]
-}
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maxSumInit' l 0
  where
    maxSumInit' :: (Num a, Ord a) => [a] -> a -> a
    maxSumInit' [] acc = acc
    maxSumInit' (h:t) acc = maxSumInit' t (h + acc)

-- 6) optimize the following recursive definition that computes the n-th number of the fibonacci sequence using an auxiliary function with 2 accumulating parameters that represent, respectively, the n-th and the (n+1)-th numbers of this sequence
{-
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-}
fib :: Int -> Int
fib n = fib' n 0 1
  where
    fib' :: Int -> Int -> Int -> Int
    fib' 0 x _ = x
    fib' n x y = fib' (n - 1) y (x + y)

-- 7) define a function that converts an Integer to a String using an auxiliary function, returning the value of an accumulating parameter on which the String will be built
intToStr :: Int -> String
intToStr n = intToStr' n []
  where
    intToStr' :: Int -> String -> String
    intToStr' 0 acc = acc
    intToStr' n acc =
        let chrLastDigit = chr ((mod n 10) + 48)
            restOfDigits = (div n 10)
         in intToStr' restOfDigits (chrLastDigit : acc)

-- 8) express the following list expressions by enumeration
-- 8.a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
list8a = [6, 12, 18]

-- 8.b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
list8b = [6, 12, 18]

-- 8.c) [(x, y) | x <- [0..20], y <- [0..20], x + y == 30]
list8c =
    [ (10, 20)
    , (11, 19)
    , (12, 18)
    , (13, 17)
    , (14, 16)
    , (15, 15)
    , (16, 14)
    , (17, 13)
    , (18, 12)
    , (19, 11)
    , (20, 10)
    ]

-- 8.d) [sum [y | y <- [1..x], odd y] | x <- [1..10]]
list8d = [1, 1, 4, 4, 9, 9, 16, 16, 25, 25]

-- 9) define each of the following lists using list comprehension
-- 9.a) [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]
list9a = [2 ^ x | x <- [0 .. 10]]

-- 9.b) [(1, 5), (2, 4), (3, 3), (4, 2), (5, 1)]
list9b = [(a, b) | a <- [1 .. 5], b <- [1 .. 5], a + b == 6]

-- 9.c) [[1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]
list9c = [y | y <- [[1 .. x] | x <- [1 .. 5]]]

-- 9.d) [[1], [1, 1], [1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1, 1]]
list9d = [[1 | _ <- [1 .. n]] | n <- [1 .. 5]]

-- 9.e) [1, 2, 6, 24, 120, 720]
list9e = [product [1 .. n] | n <- [1 .. 6]]
