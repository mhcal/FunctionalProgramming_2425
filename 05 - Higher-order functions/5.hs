-- 1) write a definition for the following higher-order functions (pre-defined on Prelude or Data.List)
-- a.0) any
any1 :: (a -> Bool) -> [a] -> Bool
any1 p (x:xs) = p x || any1 p xs
any1 _ _ = False

-- a.1) filter
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p (x:xs)
    | p x = (x : (filter1 p xs))
    | otherwise = filter1 p xs
filter1 _ _ = []

-- b) zipWith
zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f (h1:t1) (h2:t2) = ((f h1 h2) : (zipWith1 f t1 t2))
zipWith1 _ _ _ = []

-- c) takeWhile
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x:xs)
    | p x = (x : (takeWhile1 p xs))
    | otherwise = []
takeWhile1 _ _ = []

-- d) dropWhile
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p (x:xs)
    | p x = dropWhile1 p xs
    | otherwise = x : xs
dropWhile1 _ _ = []

-- e) span
span1 :: (a -> Bool) -> [a] -> ([a], [a])
span1 p (x:xs)
    | p x = (x : a, b)
    | otherwise = ([], (x : xs))
  where
    (a, b) = span p xs
span1 _ _ = ([], [])

-- f) deleteBy
deleteBy1 :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy1 p e (x:xs)
    | p e x = xs
    | otherwise = (x : deleteBy1 p e xs)
deleteBy1 _ _ [] = []

-- g) sortOn
sortOn1 :: Ord b => (a -> b) -> [a] -> [a]
sortOn1 f (x:xs) = sortOn1' f x (sortOn1 f xs)
  where
    sortOn1' :: Ord b => (a -> b) -> a -> [a] -> [a]
    sortOn1' f x [] = [x]
    sortOn1' f x (h:t) =
        if f x > f h
            then (h : sortOn1' f x t)
            else (x : h : t)
sortOn1 f _ = []

-- 2) redefine the polynomial functions from ficha 3 using higher-order functions instead of explicit recursion
type Polynomial = [Monomial]

type Monomial = (Float, Int)

-- a) selDegree (given a polynomial p, returns all monomials of p that have a specific exponent n)
selDegree :: Int -> Polynomial -> Polynomial
selDegree n p = filter ((== n) . snd) p

-- b) count (returns how many monomials of a given exponent n exist in a polynomial p)
count :: Int -> Polynomial -> Int
count n p = length $ selDegree n p

-- c) getDegree (returns the degree of a given polynomial)
getDegree :: Polynomial -> Int
getDegree p = maximum $ map snd p

-- d) derive (differentiates a polynomial)
derive :: Polynomial -> Polynomial
derive p =
    let deriveM (c, e) = (c * (fromIntegral e), e - 1)
     in map deriveM p

-- e) compute (given a specific value of x, compute the value of a given polynomial p)
compute :: Float -> Polynomial -> Float
compute x p =
    let computeM x (c, e) = ((x * c) ^ (fromIntegral e))
     in sum $ map (\y -> computeM x y) p

-- f) simp (given a polynomial p, remove all monomials of p in which the coefficient = 0)
simp :: Polynomial -> Polynomial
simp p = filter ((/= 0) . fst) p

-- g) mult (returns the product of a monomial by a polynomial)
mult :: Monomial -> Polynomial -> Polynomial
mult (mc, me) p = map (\(pc, pe) -> (mc * pc, me * pe)) p

-- h) order (given a polynomimal p, returns p ordered by ascending order of monomial exponents)
order :: Polynomial -> Polynomial
order p = sortOn1 snd p

-- i) normalize (given a polynomial p, returns a polynomial equivalent to p in which there are no monomials with the same exponent)
normalize :: Polynomial -> Polynomial
normalize p =
    let x = fromIntegral $ getDegree p
     in [((a / x), b) | (a, b) <- p]

-- j) sum (returns the sum of two polynomials p1 and p2 s.t. if p1 and p2 are both normalized, the output is also normalized)
sumP :: Polynomial -> Polynomial -> Polynomial
sumP p1 p2
    | (normalize p1 == p1) && (normalize p2 == p2) = normalize $ p1 ++ p2
    | otherwise = p1 ++ p2

-- k) product (returns the product of two polynomials)
polyproduct :: Polynomial -> Polynomial -> Polynomial
polyproduct p1 p2 = foldr mult p1 p2

-- l) equiv (tests if two polynomials are equivalent)
equiv :: Polynomial -> Polynomial -> Bool
equiv p1 p2 = order (normalize p1) == order (normalize p2)

-- 3) let the following type represent a matrix
type Mat a = [[a]]

-- define the following functions about matrices
-- a) dimOk (tests if a matrix is well-defined)
dimOk :: Mat a -> Bool
dimOk m = all (\x -> length (head m) == length x) m

-- b) dimMat (returns the dimension of a given matrix)
dimMat :: Mat a -> (Int, Int)
dimMat a = (length $ head a, length a)

-- c) addMat (adds two matrices)
addMat :: Num a => Mat a -> Mat a -> Maybe (Mat a)
addMat a b
    | dimMat a == dimMat b = Just (zipWith addVec a b)
    | otherwise = Nothing
  where
    addVec :: Num a => [a] -> [a] -> [a]
    addVec v1 v2 = zipWith (+) v1 v2

-- d) transpose (computes the transpose of a given matrix)
transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = ((map head m) : (transpose $ map tail m))

-- e) multMat (computes the product of two given matrices)
multMat :: Num a => Mat a -> Mat a -> Maybe (Mat a)
multMat a b
    | snd (dimMat a) == fst (dimMat b) =
        Just ([[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a])
    | otherwise = Nothing

-- f) zipWMat (zipWith over matrices)
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f (h1:t1) (h2:t2) = ((zipWith f h1 h2) : (zipWMat f t1 t2))

-- g) uppTri (tests if a square matrix is upper-triangular)
uppTri :: (Num a, Eq a) => Mat a -> Bool
uppTri [] = True
uppTri (x:xs) = (all (== 0) (map head xs)) && (uppTri (map tail xs))

-- h) rotateLeft (rotates a given matrix 90 degrees to the left)
rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = [[]]
rotateLeft m = ((map last m) : rotateLeft (map init m))
