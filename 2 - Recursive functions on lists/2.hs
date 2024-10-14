import Data.Char

{- 1) show each reduction step applied to the following functions and terms
1.a) funA [2,3,5,1] =>
= 2^2 + (funA [3, 5, 1]) 
= 2^2 + 3^2 + (funA [5, 1])
= 2^2 +3^2 + 5^2 + (funA [1])
= 2^2 + 3^2 + 5^2 + 1^2 + (funA [])
= 2^2 + 3^2 + 5^2 + 1^2 + 0
= 39.0 -}
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

{- 1.b) funB [8,5,12]
= 8:(funB [5,12])
= 8:(funB [12])
= 8:12:(funB [])
= 8:12:[]
= [8, 12] -}
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) | (mod h 2) == 0 = h:(funB t)
           | otherwise = (funB t)

{- 1.c) funC [1,2,3,4,5]
= funC [3, 4, 5]
= funC [5]
= [5] -}
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

{- 1.d) funD "otrec"
= g [] "otrec"
= g ('o':acc) "trec"
= g ('t':'o') "rec"
= g ('r':'t':'o') "ec"
= g ('e':'r':'t':'o') "c"
= g ('c':'e':'r':'t':'o') []
= "certo" -}
funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t


-- 2) define the following recursive list functions
-- 2.a) given an inputted list, outputs the list in which every element is 2x the entry element
doubles :: [Float] -> [Float]
doubles [] = []
doubles (h:t) = (2*h):(doubles t)

-- 2.b) computes the number of times a given character appears on a list
numOccurs :: Char -> String -> Int
numOccurs x [] = 0
numOccurs x (h:t) | (h == x) = 1 + (numOccurs x t)
                  | otherwise = numOccurs x t

-- 2.c) tests if a list only has positive elements
positives :: [Int] -> Bool
positives [] = True
positives (h:t) | (h > 0) = positives t
                | otherwise = False

-- 2.d) removes all non-positive elements from an integer list
onlyPos :: [Int] -> [Int]
onlyPos [] = []
onlyPos (h:t) | (h > 0) = h:(onlyPos t)
              | otherwise = onlyPos t

-- 2.e) adds all negative numbers in an input list
sumNeg :: [Int] -> Int
sumNeg [] = 0
sumNeg (h:t) | (h < 0) = h + (sumNeg t)
             | otherwise = sumNeg t

-- 2.f) outputs the last three elements of a list, if the input list has < 3 elements, it outputs the list itself
lastThree :: [a] -> [a]
lastThree l | length l <= 3 = l
            | otherwise = lastThree (tail l)

-- 2.g) outputs the list of the second elements from a list of pairs
seconds :: [(a, b)] -> [b]
seconds [] = []
seconds (h:t) = (snd h):(seconds t)

-- 2.h) tests if an input element appears in a first position in a list of pairs
inFirst :: (Eq a) => a -> [(a, b)] -> Bool
inFirst x [] = False
inFirst x (h:t) | (fst h) == x = True
                | otherwise = inFirst x t

-- 2.i) adds a list of triple tuples element by element
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

sumTriple :: (Num a, Num b, Num c) => [(a, b, c)] -> (a, b, c)
sumTriple [] = (0, 0, 0)
sumTriple ((x, y, z):t) = (x + fst3 (sumTriple t), y + snd3 (sumTriple t), z + thd3 (sumTriple t))


-- 3) using the functions from the module Data.Char, define the following functions using recursion
-- 3.a) inputs a list of characters and outputs the list of the characters that are not digits
onlyDigits :: [Char] -> [Char]
onlyDigits [] = []
onlyDigits (h:t) | (isDigit h == True) = h:(onlyDigits t)
                 | otherwise = onlyDigits t

-- 3.b) inputs a list of characters and outputs how many of them are lowercase
onlyLower :: [Char] -> [Char]
onlyLower [] = []
onlyLower (h:t) | (isLower h == True) = h:(onlyLower t)
                | otherwise = onlyLower t

-- 3.c) inputs a string and outputs a list with the integers respective to the digits that occur
respectiveInt :: String -> [Int]
respectiveInt [] = []
respectiveInt (h:t) | ord h >= 49 && ord h <= 57 = ((ord h) - 48):(respectiveInt t)
                    | otherwise                  = respectiveInt t


-- 4) one way to represent the polynomials of a variable is to use a list of monomials represented by pairs of type (coefficient, exponent). with that in mind, define the following functions
type Monomial = (Float, Int)
type Polynomial = [Monomial]

-- 4.a) compute how many monomials of a given power n exist in a polynomial p
count :: Int -> Polynomial -> Int
count n [] = 0
count n (h:t) | (snd h) == n = 1 + (count n t)
              | otherwise    = count n t

-- 4.b) outputs the degree of a polynomial
degree :: Polynomial -> Int
degree []    = 0
degree (h:t) = max (snd h) (degree t)

-- 4.c) returns a list of the polynomials with a given monomial exponent
selDegree :: Int -> Polynomial -> Polynomial
selDegree n [] = []
selDegree n (h:t) | snd h == n = h:(selDegree n t)
                  | otherwise  = selDegree n t

-- 4.d) computes the derivative of a polynomial
deriv :: Polynomial -> Polynomial
deriv [] = []
deriv (h:t) | snd h /= 0 = (((fst h) * fromIntegral ((snd h))), (snd h) - 1):(deriv t)
            | otherwise  = deriv t

-- 4.e) computes the value of a polynomial given a value of x
computePoly :: Float -> Polynomial -> Float
computePoly _ []    = 0
computePoly x (h:t) = (fst h * (x^(snd h))) + computePoly x t

-- 4.f) removes the monomials with coefficient = 0 from a given polynomial
simpPoly :: Polynomial -> Polynomial
simpPoly [] = []
simpPoly (h:t) | (fst h) /= 0 = h:(simpPoly t)
               | otherwise    = simpPoly t

-- 4.g) computes the product of a monomial with a polynomial
prodMonoPoly :: Monomial -> Polynomial -> Polynomial
prodMonoPoly m (h:[]) = h:(m:[])
prodMonoPoly m (h:t) | (snd m) >= (snd h) = m:(h:t)
                     | otherwise          = h:(prodMonoPoly m t)

-- 4.h) given an input polynomial, outputs an equivalent polynomial in which there aren't multiple monomials with the same exponent
normalizePoly :: Polynomial -> Polynomial
normalizePoly (h:[]) = (h:[])
normalizePoly (h:t) | (snd h) == (snd (head t)) = (fst h + fst (head t), snd h):(normalizePoly (tail t))
                    | otherwise                 = h:(normalizePoly t)

-- 4.i) adds two polynomials and, if they're normalized, it also outputs a normalized polynomial
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly [] b = b
addPoly a b | (normalizePoly a == a) || (normalizePoly b == b) = normalizePoly ((head a):(addPoly (tail a) b))
            | otherwise = (head a):(addPoly (tail a) b)
            
-- 4.j) computes the product of two polynomials
prodPoly :: Polynomial -> Polynomial -> Polynomial
prodPoly [] _ = []
prodPoly _ [] = []
prodPoly (h:t) poly2 = addPoly (prodMonoPoly h poly2) (prodPoly t poly2)

-- 4.k) sort a polynomial by the degrees of its monomial in ascending order
sortPoly :: Polynomial -> Polynomial
sortPoly (h:[]) = (h:[])
sortPoly (h:t) | (snd h) < (snd (head t)) = (head t):(sortPoly (h:(tail t)))
               | otherwise                = h:(sortPoly t)

-- 4.l) checks if two polynomials are equivalent
equivPoly :: Polynomial -> Polynomial -> Bool
equivPoly a b | normalizePoly a == normalizePoly b = True
              | otherwise = False
